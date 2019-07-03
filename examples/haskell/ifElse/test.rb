
require 'tmpdir'
require 'open3'

def genChain( first, last, i, j )
  result = ''
  result << 'begin |> ' << first
  i.times do
    result << ' |> if_ False |> then_'
  end
  result << ' |> return_ "A"'
  j.times do
    result << ' |> else_ |> return_ "A"'
  end
  result << ' |> ' << last << ' |> end'
end

def genSource( first, last, i, j )
  <<EOS
import IfElse

main :: IO ()
main = print $ #{ genChain( first, last, i, j ) }
EOS
end

for first in ['a', 'b']
  for last in ['a', 'b']
    for i in 0 ... 10
      for j in 0 ... 10
        source = genSource( first, last, i, j )
        Dir.mktmpdir do | temp |
          `cp IfElse.hs #{ temp }`
          Dir.chdir( temp ) do
            File.open( 'Test.hs', 'w' ) do | f |
              f.write( source )
            end
            msg = `ghc Test.hs 2>&1`
            result = $?.success?
            ans = first == last && i >= j
            if result == ans
              puts "#{ [ first, last, i, j ].inspect } (#{ result } == #{ ans }): ok"
            else
              puts "#{ [ first, last, i, j ].inspect } (#{ result } != #{ ans }): error"
              STDERR.puts msg
            end
          end
        end
      end
    end
  end
end
