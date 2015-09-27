import Criterion.Main
import Pipes
import Pipes.Vector
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

main :: IO ()
main = defaultMain
    [ bench "VB.Vector Int" $ whnfIO (runEffect $ runToVectorP $ each [1..100000] >-> toVector :: IO (VB.Vector Int))
    , bench "VU.Vector Int" $ whnfIO (runEffect $ runToVectorP $ each [1..100000] >-> toVector :: IO (VU.Vector Int))
    , bench "VS.Vector Int" $ whnfIO (runEffect $ runToVectorP $ each [1..100000] >-> toVector :: IO (VS.Vector Int))
    ]
