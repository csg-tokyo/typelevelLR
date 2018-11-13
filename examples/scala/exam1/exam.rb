
require 'tmpdir'
require 'benchmark'

def runshell( command )
  puts "#> #{ command }"
  `#{ command }`
end

ns = ( 1 .. 30 ).collect { | i | i * 2 }
ds = [4]

results = {}

for n in ns
  for d in ds
    Dir.mktmpdir do | dir |
      puts "case: n = #{ n }, d = #{ d }"
      puts "setup"
      runshell "cp exam1.scala #{ dir }"
      runshell "runghc makeTest.hs -n #{ n } -d #{ d }"
      runshell "mv test.scala #{ dir }"
      results[ [ n, d ] ] = Dir.chdir( dir ) do
        runshell "scalac exam1.scala"
        "setup done"
        "start warmup"
        5.times { runshell "scalac -J-Xss100m test.scala" }
        "warmup done"
        (1 .. 20).collect do
          t = Benchmark.measure do
            `scalac -J-Xss100m test.scala`
          end
          puts t.total
          t
        end
      end
    end
  end
end

puts results
