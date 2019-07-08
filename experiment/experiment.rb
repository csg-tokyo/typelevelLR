
require 'optparse'
## require 'tmpdir'
## require 'open3'
## require 'benchmark'

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

###############################################################################

## Experiment

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

  def log(msg)
    puts msg if $verbouse
  end

  def measure(&block)
    start = Time.now
    block.call
    finish = Time.now
    finish - start
  end

  def invoke
    results = []
    setup do |setting|
      log("measurament on #{ setting }")
      log("start warmup")
      $num_warmup.times do
        target(setting)
        cleanup(setting)
      end
      log("finish warmup")
      result = []
      (1 .. $num_measure).each do |i|
        log("start measurament ##{ i }")
        t =  measure { target(setting) }
        log("finish measurament ##{ i }: #{ t }")
        result << t
        cleanup(setting)
      end
      results << [setting, result]
      log("summary: #{ setting } => #{ result.inspect }")
    end
    results
  end
end

###############################################################################

class DotLanguageExperiment < Experiment
  def target
    raise NotImplementedError, "#{ self.class }#target"
  end

  def cleanup
    raise NotImplementedError, "#{ self.class }#cleanup"
  end

  def ns
    raise NotImplementedError, "#{ self.class }#ns"
  end

  def ds
    raise NotImplementedError, "#{ self.class }#ds"
  end

  def setup_library
    raise NotImplementedError, "#{ self.class }#setup_library"
  end

  def generate_program(progname, n, d)
    raise NotImplementedError, "#{ self.class }#generate_program"
  end

  def runshell(command)
    log("#> #{ command }")
    result = `#{ command }`
    unless $?.success?
      raise RuntimeError, "`#{ command }` failed"
    end
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

    syntaxfilename = find_syntax_file
    runshell("cp #{ syntaxfilename } #{ workspace_name }")

    workspace_name
  end

  def generate_chain(n, d)
    node_names = identifiers('ABCDEFGHIJKLMNOPQRSTUVWXYZ').take(n)
    node_decls = node_names.collect { |node_name| ['node', ["\"#{ node_name }\""]] }
    edge_decls = node_names.zip(node_names.rotate).flat_map do |from, to|
      [['edge', ["\"#{ from }\""]], ['to', ["\"#{ to }\""]]]
    end
    [['begin', []], ['digraph', ['"test"']]] + node_decls + edge_decls + [['end', []]]
  end

  def setup
    workspace = setup_workspace
    Dir.chdir(workspace) do
      libname = setup_library
      ns.each do |n|
        ds.each do |d|
          chain = generate_chain(n, d)
          progname = generate_program(libname, n, d, chain)
          setting = { n: n, d: d, e: chain.size, progname: progname, libname: libname }
          yield setting
        end
      end
    end
  end
end

###############################################################################

class HaskellDotLanguageExperiment < DotLanguageExperiment
  attr_reader :ns, :ds

  def initialize(ns, ds)
    @ns = ns
    @ds = ds
  end

  def compile(filename)
    runshell("ghc -O2 -fcontext-stack=2000 #{ filename }")
  end

  def target(setting)
    compile(setting[:progname])
  end

  def cleanup(setting)
    basename = File.basename(setting[:progname], '.hs')
    runshell("rm #{ basename }.hi #{ basename }.o")
  end

  def setup_library
    runshell("~/.local/bin/typelevelLR --hs")
    libname = 'DotLanguage.hs'
    compile(libname)
    libname
  end

  def generate_program(libname, n, d, chain)
    chain = chain.collect do |m, args|
      "#{ m } #{ args.join(' ') }"
    end.join(' |> ')
    program = <<EOS
import #{ File.basename( libname, '.hs' ) }

main :: IO ()
main = print $ #{ chain }
EOS
    progname = "Experiment#{ n }_#{ d }.hs"
    File.open(progname, 'w') do |file|
      file.write(program)
    end
    progname
  end
end

class ScalaDotLanguageExperiment < DotLanguageExperiment
  attr_reader :ns, :ds

  def initialize(ns, ds)
    @ns = ns
    @ds = ds
  end

  def compile(filename)
    runshell("scalac -classpath . -J-Xss100m #{ filename }")
  end

  def target(setting)
    compile(setting[:progname])
  end

  def cleanup(setting)
    basename = File.basename(setting[:progname], '.scala')
    runshell("rm experiment*.class")
  end

  def setup_library
    runshell("~/.local/bin/typelevelLR --scala")
    compile('DotLanguage.scala')
    libname = 'DotLanguage'
  end

  def generate_program(libname, n, d, chain)
    progname = "experiment#{ n }_#{ d }.scala"
    chain = chain.collect do |m, args|
      "#{ m }(#{ args.join(', ') })"
    end.join('.')
    program = <<EOS
object experiment#{ n }_#{ d } {
    import #{ libname }._

    def main(args: Array[String]) = {
        println(#{ chain })
    }
}
EOS
    File.open(progname, 'w') do |file|
      file.write(program)
    end
    progname
  end
end

class CppDotLanguageExperiment < DotLanguageExperiment
  attr_reader :ns, :ds

  def initialize(ns, ds)
    @ns = ns
    @ds = ds
  end

  def compile(filename, additional_options = '')
    runshell("g++ -O2 -std=c++17 #{ additional_options } #{ filename }")
  end

  def target(setting)
    libname = setting[:libname]
    progname = setting[:progname]
    compile("#{ progname }.cpp", '-c')
    compile("#{ libname }.o #{ progname }.o")
  end

  def cleanup(setting)
    runshell("rm #{ setting[:progname] }.o")
  end

  def setup_library
    runshell("~/.local/bin/typelevelLR --cpp")
    libname = 'DotLanguage'
    compile("#{ libname }.cpp", '-c')
    libname
  end

  def generate_program(libname, n, d, chain)
    chain = chain.collect do |m, args|
      "#{ m }(#{ args.join(', ') })"
    end.join('->')
    program = <<EOS
#include <iostream>
#include "#{ libname }.hpp"
using namespace #{ libname };

int main() {
  auto parseTree = #{ chain };
  std::cout << *parseTree << std::endl;
  return 0;
}
EOS
    progname = "experiment#{ n }_#{ d }"
    File.open("#{ progname }.cpp", 'w') do |file|
      file.write(program)
    end
    progname
  end
end

###############################################################################

opt = OptionParser.new

config = {}

opt.on('--hs', '--haskell') { config[:haskell] = true }
opt.on('--scala') { config[:scala] = true }
opt.on('--cpp') { config[:cpp] = true }
opt.on('-v', '--verbouse') { $verbouse = true }
opt.on('-n N', '--max_n N') { |n| config[:max_n] = n.to_i }
opt.on('-d D', '--max_d D') { |n| config[:max_d] = n.to_i }

opt.parse!(ARGV)

###############################################################################

case [config[:haskell], config[:scala], config[:cpp]].count(true)
when 0
  raise RuntimeError, "non of --haskell nor --scala nor --cpp is passed"
when 1
else
  raise RuntimeError, "multiple options of --haskell and --scala and --cpp are passed"
end

max_n = config[:max_n] || 100
max_d = config[:max_d] || 1

ns = 1 .. max_n
ds = 1 .. max_d

experiment = if config[:haskell]; HaskellDotLanguageExperiment.new(ns, ds)
             elsif config[:scala]; ScalaDotLanguageExperiment.new(ns, ds)
             elsif config[:cpp]; CppDotLanguageExperiment.new(ns, ds)
             end

results = experiment.invoke

puts "n, d, e, mean, variance"
results.each do |setting, result|
  puts "#{ setting[:n] }, #{ setting[:d] }, #{ setting[:e] }, #{ mean(result) }, #{ variance(result) }"
end

###############################################################################

