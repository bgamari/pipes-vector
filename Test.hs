import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Vector

main = do
    a <- runEffect $ runToVectorP $ (PP.fromList [1..5::Int] >-> toVector) ()
    print a