import Pipes
import Pipes.Vector

main = do
    a <- run $ runToVectorP $ each [1..5::Int] >-> toVector
    print a
