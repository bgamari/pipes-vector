{-# LANGUAGE TemplateHaskell #-}

module Control.Proxy.Vector ( toVectorD
                            , runToVectorD
                            ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Primitive
import           Control.Proxy
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as VU

data ToVectorState v e = ToVecS { result :: v e
                                , idx    :: Int
                                }
                       deriving (Show, Eq)

maxChunkSize = 8*1024*1024

toVectorD
    :: (PrimMonad m, Functor m, Proxy p, M.MVector v e)
    => () -> Consumer p e (StateT (ToVectorState (v (PrimState m)) e) m) r
toVectorD () = runIdentityP go where
    go = do
        pos <- lift $ do
            length <- M.length . result <$> get
            pos    <- idx <$> get
            when (pos >= length) $ do
                v  <- result <$> get
                v' <- lift $ M.unsafeGrow v (min length maxChunkSize)
                modify $ \(ToVecS r i)->ToVecS v' i
            return pos
        r <- request ()
        lift $ do
            v <- result <$> get
            lift $ M.unsafeWrite v pos r
            modify $ \(ToVecS r i)->ToVecS r (pos+1)
        go

runToVectorD
    :: (PrimMonad m, MVU.Unbox e)
    => StateT (ToVectorState (MVU.MVector (PrimState m)) e) m r
    -> m (VU.Vector e)
runToVectorD x = do
    v      <- MVU.new 10
    (_, s) <- flip runStateT (ToVecS v 0) x
    liftM (VU.take (idx s)) $ V.freeze $ result s
