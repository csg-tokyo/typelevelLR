
require 'optparse'

###############################################################################

## configuration

$verbouse = false
$num_warmup = 3
$num_measure = 10

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
  result
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
    (1 .. 200)
  end

  def default_ms
    (1 .. 10)
  end

  def setup
    syntaxfile = find_syntax_file
    runshell("typelevelLR --#{ lang }")
    libname = runshell("./GenRandomChainCI show-#{ lang }-libname #{ syntaxfile }").strip
    ns.each do |n|
      ms.each do |m|
        filename = "#{ main }_n#{ n }_m#{ m }#{ ext }"
        syntaxfile = find_syntax_file
        begin
          unless File.exists?(filename)
            runshell("./GenRandomChainCI gen-#{ lang }-chain -n #{ n } -o #{ filename } #{ syntaxfile }")
          end
          setting = { n: n, m: m, filename: filename, syntaxfile: syntaxfile, libname: libname }
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
      raise RuntimeError, "file not found -- GenRandomChainCI"
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
    runshell("ghc -O2 -fcontext-stack=2000 #{ filename }")
  end

  def cleanup(setting)
    basename = File.basename(setting[:filename], '.hs')
    runshell("rm #{ basename } #{ basename }.hi #{ basename }.o")
  end
end

class ScalaRandomChainExperiment < RandomChainExperiment
  def lang
    'scala'
  end

  def main
    'main'
  end

  def ext
    '.scala'
  end

  def compile(filename)
    runshell("scalac -sourcepath . -J-Xss100m #{ filename }")
  end

  def cleanup(setting)
    basename = File.basename(setting[:filename], '.scala')
    runshell("rm #{ basename }.class #{ basename }$.class")
  end
end

class CppRandomChainExperiment < RandomChainExperiment
  def lang
    'cpp'
  end

  def main
    'main'
  end

  def ext
    '.cpp'
  end

  def target(setting)
    filename = setting[:filename]
    basename = File.basename(filename, '.cpp')
    libname  = setting[:libname ]
    unless File.exists?("#{ libname }.o")
      compile("#{ libname }.cpp", ['-c'])
    end
    compile("#{ basename }.cpp", ['-c'])
    compile("#{ libname }.o #{ basename }.o", ["-o #{ basename }"])
  end

  def compile(filename, additional_options = [])
    runshell("g++ -O2 -std=c++17 #{ additional_options.join(' ') } #{ filename }")
  end

  def cleanup(setting)
    filename = setting[:filename]
    basename = File.basename(filename, '.cpp')
    runshell("rm #{ basename } #{ basename }.o")
  end
end

###############################################################################

opt = OptionParser.new

config = {}

opt.on('--hs', '--haskell') { config[:haskell] = true }
opt.on('--scala') { config[:scala] = true }
opt.on('--cpp') { config[:cpp] = true }
opt.on('-v', '--verbouse') { $verbouse = true }
opt.on('-n N', '--max-n N') { |n| config[:max_n] = n.to_i }
opt.on('-m M', '--max-m M') { |n| config[:max_m] = n.to_i }
opt.on('--num-warmup N') { |n| $num_warmup = n.to_i }
opt.on('--num-measure N') { |n| $num_measure = n.to_i }

opt.parse!(ARGV)

###############################################################################

case [config[:haskell], config[:scala], config[:cpp]].count(true)
when 0
  raise RuntimeError, "non of --haskell nor --scala nor --cpp is passed"
when 1
else
  raise RuntimeError, "multiple options of --haskell and --scala and --cpp are passed"
end

max_n = config[:max_n] || 200
max_m = config[:max_m] || 10

ns = 1 .. max_n
ms = 1 .. max_m

experiment = if config[:haskell]; HaskellRandomChainExperiment.new(ns, ms)
             elsif config[:scala]; ScalaRandomChainExperiment.new(ns, ms)
             elsif config[:cpp]; CppRandomChainExperiment.new(ns, ms)
             end

results = experiment.invoke

puts "n, m, mean, variance"
results.each do |setting, ts|
  puts "#{ setting[:n] }, #{ setting[:m] }, #{ mean(ts) }, #{ variance(ts) }"
end

###############################################################################
