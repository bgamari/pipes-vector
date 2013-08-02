import Pipes
import Pipes.Vector
import qualified Data.Vector as V

main = do
    a <- run $ runToVectorP $ each [1..5] >-> toVector :: IO (V.Vector Int)
    print a
