{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Data.HKD
import GHC.Generics (Generic)

data Foo = Foo | Bar | Baz

data MyHkd (f :: Foo -> *) = MyHkd
  { foo :: f 'Foo
  , bar :: f 'Bar
  , baz :: f 'Baz
  }
  deriving Generic

instance FFunctor MyHkd where ffmap = ffmapDefault
instance FFoldable MyHkd where ffoldMap = ffoldMapDefault
instance FTraversable MyHkd where ftraverse = gftraverse
instance FZip MyHkd where fzipWith = gfzipWith
instance FRepeat MyHkd where frepeat = gfrepeat

main :: IO ()
main = return ()
