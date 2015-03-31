{-# LANGUAGE RankNTypes, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

{-| Pipes for interfacing with "Data.Vector".

    Note that this only provides functionality for building @Vectors@
    from Pipes; as @Vectors@ are @Foldable@ the inverse can be
    accomplished with "Pipes.each".
-}

module Pipes.Vector (
    -- * Usage
    -- $usage
    -- * Building Vectors from Pipes
    toVector,
    runToVectorP,
    runToVector,
    fromProducer,
    ToVector
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Primitive
import qualified Control.Monad.Primitive.Class as MP
import Pipes
import Pipes.Internal (unsafeHoist)
import Pipes.Lift
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M

data ToVectorState v e m = ToVecS { result :: V.Mutable v (PrimState m) e
                                  , idx :: Int
                                  }

newtype ToVector v e m r = TV {unTV :: S.StateT (ToVectorState v e m) m r}
                         deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ToVector v e) where
    lift = TV . lift

instance PrimMonad m => PrimMonad (ToVector v e m) where
    type PrimState (ToVector v e m) = PrimState m
    primitive = lift . primitive

-- Nasty orphan instances
instance PrimMonad m => PrimMonad (Proxy a' a b' b m) where
    type PrimState (Proxy a' a b' b m) = PrimState m
    primitive = lift .  primitive

-- monad-primitive instances (TODO: remove these)
instance MP.MonadPrim m => MP.MonadPrim (Proxy a' a b' b m) where
    type BasePrimMonad (Proxy a' a b' b m) = MP.BasePrimMonad m
    liftPrim = lift . MP.liftPrim
  
instance MP.MonadPrim m => MP.MonadPrim (ToVector v e m) where
    type BasePrimMonad (ToVector v e m) = MP.BasePrimMonad m
    liftPrim = TV . MP.liftPrim
                         
maxChunkSize :: Int
maxChunkSize = 8*1024*1024

-- | Consume items from a Pipe and place them into a vector
--
-- For efficient filling, the vector is grown geometrically up to a
-- maximum chunk size.
toVector
     :: (PrimMonad m, M.MVector (V.Mutable v) e)
     => Consumer e (ToVector v e m) r
toVector = forever $ do
      length <- M.length . result <$> lift (TV get)
      pos <- idx <$> lift (TV get)
      lift $ TV $ when (pos >= length) $ do
          v <- result <$> get
          v' <- M.unsafeGrow v (min length maxChunkSize)
          modify $ \(ToVecS r i) -> ToVecS v' i
      r <- await
      lift $ TV $ do
          v <- result <$> get
          M.unsafeWrite v pos r
          modify $ \(ToVecS r i) -> ToVecS r (pos+1)

-- | Extract and freeze the constructed vector
runToVectorP
     :: (PrimMonad m, V.Vector v e)
     => Proxy a' a b' b (ToVector v e m) r
     -> Proxy a' a b' b m (v e)
runToVectorP x = do
     v <- M.new 10
     s <- execStateP (ToVecS v 0) (hoist unTV x)
     frozen <- V.freeze (result s)
     return $ V.take (idx s) frozen

runToVector :: (PrimMonad m, V.Vector v e)
            => ToVector v e m r -> m (v e)
runToVector (TV a) = do
     v <- M.new 10
     s <- execStateT a (ToVecS v 0)
     frozen <- V.freeze (result s)
     return $ V.take (idx s) frozen

{- $usage

   >>> run $ runToVectorP $ each [1..5::Int] >-> toVector
   fromList [1,2,3,4,5]

-}

fromProducer :: (V.Vector v e, PrimMonad m) => Producer e (ToVector v e m) r -> m (v e)
fromProducer p = runEffect $ runToVectorP (p >-> toVector)

