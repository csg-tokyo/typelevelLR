
###############################################################################

## configuration

$verbouse = false
$num_warmup = 5
$num_measure = 20

###############################################################################

## utility functions

def mean(xs)
  xs = Array(xs)
  xs.reduce(0.0, &:+) / xs.size
end

def variance(xs)
  xs = Array(xs)
  m = mean(xs)
  mean(xs.collect { |x| (x - m) ** 2 })
end

def identifiers(alphabets = 'abcdefghijklmnopqrstuvwxyz', &block)
  return to_enum(:identifiers, alphabets) if block.nil?
  alphabets.each_char(&block)
  identifiers(alphabets) do |prefix|
    alphabets.each_char do |suffix|
      block.call(prefix + suffix)
    end
  end
end

def log(message)
  puts message if $verbouse
end

def runshell(command)
  log("$> #{ command }")
  result = `#{ command }`
  unless $?.success?
    raise RuntimeError, "`#{ command }` failed"
  end
end

###############################################################################

class Experiment
  def target(setting)
    raise NotImplementedError, "#{ self.class }#target"
  end

  def cleanup(setting)
    raise NotImplementedError, "#{ self.class }#cleanup"
  end

  def setup(&block)
    raise NotImplementedError, "#{ self.class }#setup"
  end

  def measure(&block)
    start = Time.now
    block.call
    finish = Time.now
    finish - start
  end

  def invoke
    results = []
    workspace_name = setup_workspace

    Dir.chdir(workspace_name) do
      setup do |setting|
        log("measurament on #{ setting }")
        if $num_warmup > 0
          log("start warmup")
          $num_warmup.times do
            target(setting)
            cleanup(setting)
          end
        end
        log("finish warmup")
        measuraments = []
        (1 .. $num_measure).each do |i|
          log("start measurament ##{ i }")
          t = measure { target(setting) }
          log("finish measurament ##{ i }: #{ t }")
          measuraments << t
          cleanup(setting)
        end
        results << [setting, measuraments]
        log("summary: #{ setting } => #{ measuraments }")
      end
      results
    end
  end
end

###############################################################################

class RandomChainExperiment < Experiment
  attr_reader :ns, :ms

  def initialize(ns, ms)
    @ns = ns || default_ns
    @ms = ms || default_ms
  end

  def default_ns
    (1 .. 300)
  end

  def default_ms
    (1 .. 10)
  end

  def setup
    runshell("typelevelLR --haskell")
    ## runshell("ghc -O2 -fcontext-stack=2000 SQL.hs")
    ns.each do |n|
      ms.each do |m|
        filename = "#{ main }-n#{ n }-m#{ m }#{ ext }"
        syntaxfile = find_syntax_file
        begin
          unless File.exists?(filename)
            runshell("./GenRandomChainCI --#{ lang } -n #{ n } -o #{ filename } #{ syntaxfile }")
          end
          setting = { n: n, m: m, filename: filename, syntaxfile: syntaxfile }
          yield setting
        rescue RuntimeError
          log("fail to generate random chain (length of #{ n })")
        end
      end
    end
  end

  def target(setting)
    compile(setting[:filename])
  end

  def find_syntax_file
    candidates = Dir.entries('.').grep(/\.syntax$/)
    if candidates.empty?
      raise RuntimeError, 'no syntax file found'
    elsif candidates.size > 1
      raise RuntimeError, 'multiple syntax files found'
    end
    candidates[0]
  end

  def setup_workspace
    workspace_name = "WORKSPACE_#{ self.class.name }"
    unless File.exists?(workspace_name)
      Dir.mkdir(workspace_name)
    end
    unless File.directory?(workspace_name)
      raise RuntimeError, 'workspace exists, but not a directory'
    end

    syntaxfile = find_syntax_file
    runshell("cp #{ syntaxfile } #{ workspace_name }/")
    unless File.exists?('./GenRandomChainCI')
      runshell("ghc -O2 -i.:../src GenRandomChainCI.hs")
    end
    runshell("cp GenRandomChainCI #{ workspace_name }/")

    workspace_name
  end
end

###############################################################################

class HaskellRandomChainExperiment < RandomChainExperiment
  def lang
    'haskell'
  end

  def main
    'Main'
  end

  def ext
    '.hs'
  end

  def compile(filename)
    runshell("ghc -O2 -Wno-simplifiable-class-constraints -fcontext-stack=2000 #{ filename }")
  end

  def cleanup(setting)
    basename = File.basename(setting[:filename], '.hs')
    runshell("rm #{ basename } #{ basename }.hi #{ basename }.o")
  end
end

module Scala
end

module Cpp
end

###############################################################################

results = HaskellRandomChainExperiment.new(1 .. 5, [1]).invoke

results.each do |setting, ts|
  puts "#{ setting[:n] }, #{ setting[:m] }, #{ mean(ts) }, #{ variance(ts) }"
end

###############################################################################
