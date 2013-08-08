import Pipes
import Pipes.Vector
import qualified Data.Vector as V

main = do
    -- Extracting elements from a Pipe into a Vector
    a <- run $ runToVectorP $ each [1..5] >-> toVector :: IO (V.Vector Int)
    print a

    -- Pushing elements from a Vector into a Pipe is just a standard
    -- Pipes for-each loop
    run $ for (each $ V.enumFromTo 1 5) (lift . print)
    
