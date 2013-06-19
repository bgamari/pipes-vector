module Control.Proxy.Vector ( toVectorD, runToVectorK, runToVectorP ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Proxy
import           Control.Proxy.Trans.State as S
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as VU

data ToVectorState v m e = ToVecS { result :: v (PrimState m) e
                                  , idx    :: Int
                                  }

maxChunkSize = 8*1024*1024

toVectorD
    :: (PrimMonad m, Proxy p, M.MVector v e)
    => () -> Consumer (S.StateP (ToVectorState v m e) p) e m r
toVectorD () = forever $ do
     length <- M.length . result <$> get
     pos <- idx `liftM` get
     when (pos >= length) $ do
         v <- result `liftM` get
         v' <- lift $ M.unsafeGrow v (min length maxChunkSize)
         modify $ \(ToVecS r i) -> ToVecS v' i
     r <- request ()
     v <- result `liftM` get
     lift $ M.unsafeWrite v pos r
     modify $ \(ToVecS r i) -> ToVecS r (pos+1)

runToVectorK
    :: (PrimMonad m, MVU.Unbox e, Monad (p a' a b' b m), Proxy p, MonadTrans (p a' a b' b))
    => (q -> StateP (ToVectorState VU.MVector m e) p a' a b' b m r)
    -> (q -> p a' a b' b m (VU.Vector e))
runToVectorK k q = runToVectorP (k q)

runToVectorP
    :: (PrimMonad m, MVU.Unbox e, Monad (p a' a b' b m), Proxy p, MonadTrans (p a' a b' b))
    => StateP (ToVectorState VU.MVector m e) p a' a b' b m r -> p a' a b' b m (VU.Vector e)
runToVectorP x = do
    v <- lift $ MVU.new 10
    s <- execStateP (ToVecS v 0) x
    frozen <- lift $ V.freeze (result s)
    return $ VU.take (idx s) frozen