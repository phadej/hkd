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
module Data.HKD.Yoneda (
FYoneda (..),
liftFYoneda,
lowerFYoneda,
) where

import Data.HKD

newtype FYoneda t f = FYoneda { runFYoneda :: forall r. (f ~> r) -> t r }

liftFYoneda :: FFunctor f => f a -> FYoneda f a
liftFYoneda a = FYoneda (\f -> ffmap f a)

lowerFYoneda :: FYoneda f a -> f a
lowerFYoneda (FYoneda f) = f id

instance FFunctor (FYoneda f) where
    ffmap f m = FYoneda (\k -> runFYoneda m (k . f))

instance FFoldable f => FFoldable (FYoneda f) where
    ffoldMap f = ffoldMap f . lowerFYoneda
    flengthAcc acc = flengthAcc acc . lowerFYoneda

instance FTraversable f => FTraversable (FYoneda f) where
    ftraverse f = fmap liftFYoneda . ftraverse f . lowerFYoneda

instance FZip f => FZip (FYoneda f) where
    fzipWith nt (FYoneda m) (FYoneda n) = FYoneda $ \f ->
        fzipWith (\a b -> f (nt a b)) (m id) (n id)

instance FRepeat f => FRepeat (FYoneda f) where
    frepeat a = FYoneda (\f -> frepeat (f a))
