{-# LANGUAGE TemplateHaskell #-}

module Control.Proxy.Vector ( toVectorD
                            , runToVectorD
                            ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Proxy
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as VU

data ToVectorState v e = ToVecS { _result :: v e
                                , _idx    :: Int
                                }
                       deriving (Show, Eq)
makeLenses ''ToVectorState

maxChunkSize = 8*1024*1024

toVectorD
    :: (PrimMonad m, Proxy p, M.MVector v e)
    => () -> Consumer p e (StateT (ToVectorState (v (PrimState m)) e) m) r
toVectorD () = runIdentityP go where
    go = do
        pos <- lift $ do
            length <- uses result M.length
            pos    <- use idx
            when (pos >= length) $ do
                v  <- use result
                v' <- lift $ M.unsafeGrow v (min length maxChunkSize)
                result .= v'
            return pos
        r <- request ()
        lift $ do
            v <- use result
            lift $ M.unsafeWrite v pos r
            idx .= pos + 1
        go

runToVectorD
    :: (PrimMonad m, MVU.Unbox e)
    => StateP (ToVectorState (MVU.MVector (PrimState m)) e) m r
    -> m (VU.Vector e)
runToVectorD x = do
    v      <- MVU.new 10
    (_, s) <- flip runStateT (ToVecS v 0) x
    liftM (VU.take (s^.idx)) $ V.freeze $ s^.result
