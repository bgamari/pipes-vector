import Control.Proxy       
import Control.Proxy.Vector

main = do
    a <- runProxy $ runToVectorK $ fromListS [1..5::Int] >-> toVectorD
    print a