
require 'open3'

def genChain( i, j )
  result = ''
  result << 'begin()'
  i.times do
    result << '.if_(false).then_()'
  end
  result << '.return_("A")'
  j.times do
    result << '.else_().return_("A")'
  end
  result << '.end()'
end

def genSource( i, j )
  <<EOS
    object test {
      import ifElse._
      def main( args : Array[ String ] ) = {
        val tree : Expr = #{ genChain( i, j ) }
        println( tree )
      }
    }
EOS
end

for i in 0 ... 10
  for j in 0 ... 10
    source = genSource( i, j )
    `mkdir temp`
    begin
      `cp ifElse.scala temp`
      Dir.chdir( 'temp' ) do
        File.open( 'test.scala', 'w' ) do | f |
          f.write( source )
        end
        ii, oo, ee, th = Open3.popen3( 'scalac -classpath . -sourcepath . test.scala' )
        ii.close
        th.join
        result = th.value.success?
        ans = i >= j
        if result == ans
          puts "#{ [ i, j ].inspect } (#{ result } == #{ ans }): ok"
        else
          puts "#{ [ i, j ].inspect } (#{ result } != #{ ans }): error"
          STDERR.puts ee.read
        end
      end
    ensure
      `rm -r temp`
    end
  end
end
