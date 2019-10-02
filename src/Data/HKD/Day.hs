{-# language CPP #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language Trustworthy #-}
{-# language TypeOperators #-}
module Data.HKD.Day (
FDay (..),
) where

import Data.HKD
import Data.Functor.Product

data FDay t s f = forall g h. FDay (t g) (s h) (forall x. g x -> h x -> f x)

instance FFunctor (FDay t s) where
    ffmap f (FDay tg sh g) = FDay tg sh (\a b -> f (g a b))

instance (FZip t, t ~ s) => FZip (FDay t s) where
    fzipWith f (FDay x y z) (FDay u v w) = FDay
        (fzipWith Pair x u)
        (fzipWith Pair y v)
        (\(Pair a b) (Pair c d) -> f (z a c) (w b d))

data Curried t s f = forall g.
    Curried { runCurried :: forall r. (forall x. g x -> f x -> r x) -> t g -> s r }

liftCurried :: FZip t => t f -> Curried t t f
liftCurried x = Curried $ \nt y -> fzipWith nt y x

lowerCurried :: FRepeat t => Curried t s f -> s f
lowerCurried (Curried f) = f (\_ x -> x) (frepeat undefined)
