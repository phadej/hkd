{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Type)
#else
#define Type *
#endif

import Data.HKD
import Control.Applicative as A (Applicative (pure), liftA2)
import Data.Monoid as Mon (Monoid (..))

-- We can define flipped NP (as in sop-code), which would be instance
-- of classes in Data.HKD

data NP (xs :: [k]) (f :: k -> Type) where
    Nil  :: NP '[] f
    (:*) :: f x -> NP xs f -> NP (x ': xs) f

instance FFunctor (NP xs) where
    ffmap _ Nil       = Nil
    ffmap f (x :* xs) = f x :* ffmap f xs

instance FFoldable (NP xs) where
    ffoldMap _ Nil       = Mon.mempty
    ffoldMap f (x :* xs) = mappend (f x) (ffoldMap f xs)

    flengthAcc !acc Nil       = acc
    flengthAcc !acc (_ :* xs) = flengthAcc acc xs

instance FTraversable (NP xs) where
    ftraverse _ Nil       = A.pure Nil
    ftraverse f (x :* xs) = liftA2 (:*) (f x) (ftraverse f xs)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = return ()
