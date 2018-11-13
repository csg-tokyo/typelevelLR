
require 'open3'

corrects = [ [ 'a', 'g', 'c' ],
             [ 'a', 'g', 'd' ],
             [ 'b', 'g', 'd' ],
             [ 'b', 'g', 'c' ] ]

def compile( sourcestring )
  `mkdir temp`
  `cp notSLR.scala temp`
  begin
    Dir.chdir( 'temp' ) do
      File.open( 'temp.scala', 'w' ) do | file |
        file.write( sourcestring )
      end
      Open3.popen3( "scalac -classpath . -sourcepath . temp.scala" ) do | i, o, e, th |
        i.close
        th.join
        th.value.success?
      end
    end
  ensure
    `rm -r temp`
  end
end

['a', 'b', 'c', 'd', 'g'].permutation( 3 ).each do | x, y, z |
  result = compile( <<EOS )
    object test {
      import notSLR._
      def main( args : Array[ String ] ) = {
        val tree : S = begin().#{ x }().#{ y }().#{ z }().end()
        println( tree )
      }
    }
EOS
  ans = corrects.member?( [ x, y, z ] )
  if result == ans
    puts "#{ [ x, y, z ].inspect } (#{ result } == #{ ans }): ok"
  else
    puts "#{ [ x, y, z ].inspect } (#{ result } != #{ ans }): error"
  end
end
