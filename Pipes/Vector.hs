{-# LANGUAGE RankNTypes #-}              

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
    runToVectorP
    ) where
                
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Primitive
import Pipes
import Pipes.Lift
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as VU

data ToVectorState v m e = ToVecS { result :: v (PrimState m) e
                                  , idx :: Int
                                  }

maxChunkSize = 8*1024*1024

-- | Consume items from a Pipe and place them into a vector
toVector
     :: (PrimMonad m, Functor m, M.MVector v e)
     => Consumer e (S.StateT (ToVectorState v m e) m) r
toVector = forever $ do
      length <- M.length . result <$> lift get
      pos <- idx <$> lift get
      lift $ when (pos >= length) $ do
          v <- result <$> get
          v' <- lift $ M.unsafeGrow v (min length maxChunkSize)
          modify $ \(ToVecS r i) -> ToVecS v' i
      r <- await
      lift $ do
          v <- result <$> get
          lift $ M.unsafeWrite v pos r
          modify $ \(ToVecS r i) -> ToVecS r (pos+1)

-- | Extract and freeze the constructed vector
runToVectorP
     :: (PrimMonad m, MVU.Unbox e)
     => Proxy a' a b' b (StateT (ToVectorState VU.MVector m e) m) r
     -> Proxy a' a b' b m (VU.Vector e)
runToVectorP x = do
     v <- lift $ MVU.new 10
     s <- execStateP (ToVecS v 0) x
     frozen <- lift $ V.freeze (result s)
     return $ VU.take (idx s) frozen

{- $usage

   >>> run $ runToVectorP $ each [1..5::Int] >-> toVector
   fromList [1,2,3,4,5]

-}
