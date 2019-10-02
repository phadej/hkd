{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,10,0)
module Main (main) where

main :: IO ()
main = return ()

#else
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Main (
  main,
  Record (..),
  Cons (..),
  MyU1 (..),
  MyV1,
  ) where

import Data.Kind (Type)

import GHC.Generics (Generic)
import Data.HKD
import Data.Some (Some, mkSome)

data Record f = Record
    { fieldInt    :: f Int
    , fieldString :: f String
    }
  deriving (Generic)

instance FFunctor     Record where ffmap     = ffmapDefault
instance FFoldable    Record where ffoldMap  = ffoldMapDefault
instance FTraversable Record where ftraverse = gftraverse

-------------------------------------------------------------------------------
-- Sum
-------------------------------------------------------------------------------

data Cons f = ConInt (f Int) | ConString (f String)
  deriving (Generic)

instance FFunctor     Cons where ffmap     = ffmapDefault
instance FFoldable    Cons where ffoldMap  = ffoldMapDefault
instance FTraversable Cons where ftraverse = gftraverse

-------------------------------------------------------------------------------
-- Units
-------------------------------------------------------------------------------

data MyU1 (f :: Type -> Type) = MyU1 deriving Generic
data MyV1 (f :: Type -> Type)        deriving Generic

instance FFunctor     MyU1 where ffmap     = ffmapDefault
instance FFoldable    MyU1 where ffoldMap  = ffoldMapDefault
instance FTraversable MyU1 where ftraverse = gftraverse

instance FFunctor     MyV1 where ffmap     = ffmapDefault
instance FFoldable    MyV1 where ffoldMap  = ffoldMapDefault
instance FTraversable MyV1 where ftraverse = gftraverse

-------------------------------------------------------------------------------
-- Interesting
-------------------------------------------------------------------------------

data List f = Nil | Cons (Some f) (List f) deriving Generic

instance FFunctor     List where ffmap     = ffmapDefault
instance FFoldable    List where ffoldMap  = ffoldMapDefault
instance FTraversable List where ftraverse = gftraverse

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = print $ flength
    $ Cons (mkSome (Just 'x'))
    $ Cons (mkSome (Just True))
      Nil
#endif
