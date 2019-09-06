{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language Trustworthy #-}
{-# language GADTs #-}
{-# language CPP #-}
{-# language PolyKinds #-}
#if !defined(HLINT) && MIN_VERSION_base(4,10,0) && __GLASGOW_HASKELL__ >= 708
{-# language LambdaCase #-}
{-# language EmptyCase #-}
#endif
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- "Higher-Kinded Data" such as it is
module Data.HKD
( type (~>)
-- * Functor
, FFunctor(..)
-- * Contravariant
, FContravariant(..)
-- * Foldable
, FFoldable(..)
, flength
, ftraverse_
, ffor_
-- * Traversable
, FTraversable(..)
, ffoldMapDefault
, ffmapDefault
, ffor
-- * Utilities
, Logarithm(..)
, Tab(..)
, indexLogarithm
, Element(..)
, NT(..)
, Some(..)
, Lim(..)
) where

import Control.Applicative
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy (..))
import Data.Functor.Identity (Identity (..))

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
#endif

-- In older base:s types aren't PolyKinded
#if MIN_VERSION_base(4,9,0)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
#endif

#if MIN_VERSION_base(4,10,0)
import GHC.Generics
#endif

#if MIN_VERSION_base(4,9,0)
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f

infixr 9 #.
infixr 8 .#
#endif

type f ~> g = forall a. f a -> g a

-- * FFunctor

class FFunctor t where
  ffmap :: (f ~> g) -> t f -> t g

instance FFunctor Proxy where
  ffmap _ Proxy = Proxy

#if MIN_VERSION_base(4,9,0)
instance FFunctor (Const a) where
  ffmap _ (Const a) = Const a

instance (Functor f, FFunctor g) => FFunctor (Compose f g) where
  ffmap f = Compose #. fmap (ffmap f) .# getCompose

instance (FFunctor f, FFunctor g) => FFunctor (Product f g) where
  ffmap f (Pair g h) = Pair (ffmap f g) (ffmap f h)

instance (FFunctor f, FFunctor g) => FFunctor (Sum f g) where
  ffmap f (InL g) = InL (ffmap f g)
  ffmap f (InR h) = InR (ffmap f h)
#endif

#if MIN_VERSION_base(4,10,0)
instance FFunctor (K1 i a) where
  ffmap _ (K1 a) = K1 a

instance FFunctor U1 where
  ffmap _ U1 = U1

instance FFunctor V1 where
#ifndef HLINT
  ffmap _ = \case
#endif

instance (Functor f, FFunctor g) => FFunctor (f :.: g) where
  ffmap f = Comp1 #. fmap (ffmap f) .# unComp1

instance (FFunctor f, FFunctor g) => FFunctor (f :*: g) where
  ffmap f (g :*: h) = ffmap f g :*: ffmap f h

instance (FFunctor f, FFunctor g) => FFunctor (f :+: g) where
  ffmap f (L1 g) = L1 (ffmap f g)
  ffmap f (R1 h) = R1 (ffmap f h)
#endif

class FFoldable t where
  ffoldMap :: Monoid m => (forall a. f a -> m) -> t f -> m

  flengthAcc :: Int -> t f -> Int
  flengthAcc acc t = acc + Monoid.getSum (ffoldMap (\_ -> Monoid.Sum 1) t)

flength :: FFoldable t => t f -> Int
flength = flengthAcc 0

ftraverse_ :: (FFoldable t, Applicative m) => (forall a. f a -> m b) -> t f -> m ()
ftraverse_ k tf = case ffoldMap (Some . k) tf of
  Some mx -> () <$ mx

ffor_ :: (FFoldable t, Applicative m) => t f -> (forall a. f a -> m b) -> m ()
ffor_ tf k = ftraverse_ k tf

instance FFoldable Proxy where
  ffoldMap _ = mempty
  flengthAcc = const

#if MIN_VERSION_base(4,9,0)
instance FFoldable (Const a) where
  ffoldMap _ = mempty
  flengthAcc = const

instance (Foldable f, FFoldable g) => FFoldable (Compose f g) where
  ffoldMap f = foldMap (ffoldMap f) .# getCompose

instance (FFoldable f, FFoldable g) => FFoldable (Product f g) where
  ffoldMap f (Pair g h) = ffoldMap f g `mappend` ffoldMap f h
  flengthAcc f (Pair g h) = f `flengthAcc` g `flengthAcc` h

instance (FFoldable f, FFoldable g) => FFoldable (Sum f g) where
  ffoldMap f (InL g) = ffoldMap f g
  ffoldMap f (InR h) = ffoldMap f h
#endif

#if MIN_VERSION_base(4,10,0)
instance FFoldable V1 where
#ifndef HLINT
  ffoldMap _ = \case
  flengthAcc _ = \case
#endif

instance FFoldable (K1 i a) where
  ffoldMap _ = mempty
  flengthAcc = const

instance FFoldable U1 where
  ffoldMap _ = mempty
  flengthAcc = const

instance (Foldable f, FFoldable g) => FFoldable (f :.: g) where
  ffoldMap f = foldMap (ffoldMap f) .# unComp1

instance (FFoldable f, FFoldable g) => FFoldable (f :*: g) where
  ffoldMap f (g :*: h) = ffoldMap f g `mappend` ffoldMap f h
  flengthAcc acc (g :*: h) = acc `flengthAcc` g `flengthAcc` h

instance (FFoldable f, FFoldable g) => FFoldable (f :+: g) where
  ffoldMap f (L1 g) = ffoldMap f g
  ffoldMap f (R1 h) = ffoldMap f h
  flengthAcc acc (L1 g) = flengthAcc acc g
  flengthAcc acc (R1 g) = flengthAcc acc g
#endif

-- * FTraversable

class (FFoldable t, FFunctor t) => FTraversable t where
  ftraverse :: Applicative m => (forall a. f a -> m (g a)) -> t f -> m (t g)
ffmapDefault :: FTraversable t =>  (f ~> g) -> t f -> t g
ffmapDefault k = runIdentity . ftraverse (Identity . k)

ffoldMapDefault :: (FTraversable t, Monoid m) =>  (forall a. f a -> m) -> t f -> m
ffoldMapDefault k = getConst . ftraverse (Const . k)

ffor :: (FTraversable t, Applicative m) => t f -> (forall a. f a -> m (g a)) -> m (t g)
ffor tf k = ftraverse k tf

instance FTraversable Proxy where
  ftraverse _ Proxy = pure Proxy

#if MIN_VERSION_base(4,9,0)
instance FTraversable (Const a) where
  ftraverse _ = pure .# (Const . getConst)

instance (Traversable f, FTraversable g) => FTraversable (Compose f g) where
  ftraverse f = fmap Compose . traverse (ftraverse f) .# getCompose

instance (FTraversable f, FTraversable g) => FTraversable (Product f g) where
  ftraverse f (Pair g h) = Pair <$> ftraverse f g <*> ftraverse f h

instance (FTraversable f, FTraversable g) => FTraversable (Sum f g) where
  ftraverse f (InL g) = InL <$> ftraverse f g
  ftraverse f (InR h) = InR <$> ftraverse f h
#endif

#if MIN_VERSION_base(4,10,0)
instance FTraversable U1 where
  ftraverse _ U1 = pure U1

instance FTraversable V1 where
#ifndef HLINT
  ftraverse _ = \case
#endif

instance FTraversable (K1 i a) where
  ftraverse _ = pure .# (K1 . unK1)

instance (Traversable f, FTraversable g) => FTraversable (f :.: g) where
  ftraverse f = fmap Comp1 . traverse (ftraverse f) .# unComp1

instance (FTraversable f, FTraversable g) => FTraversable (f :*: g) where
  ftraverse f (g :*: h) = (:*:) <$> ftraverse f g <*> ftraverse f h

instance (FTraversable f, FTraversable g) => FTraversable (f :+: g) where
  ftraverse f (L1 g) = L1 <$> ftraverse f g
  ftraverse f (R1 h) = R1 <$> ftraverse f h
#endif

-- * FContravariant

class FContravariant t where
  fcontramap :: (f ~> g) -> t g -> t f

instance FContravariant Proxy where
  fcontramap _ Proxy = Proxy

#if MIN_VERSION_base(4,9,0)
instance FContravariant (Const a) where
  fcontramap _ (Const a) = Const a

instance (Functor f, FContravariant g) => FContravariant (Compose f g) where
  fcontramap f = Compose #. fmap (fcontramap f) .# getCompose

instance (FContravariant f, FContravariant g) => FContravariant (Product f g) where
  fcontramap f (Pair g h) = Pair (fcontramap f g) (fcontramap f h)

instance (FContravariant f, FContravariant g) => FContravariant (Sum f g) where
  fcontramap f (InL g) = InL (fcontramap f g)
  fcontramap f (InR h) = InR (fcontramap f h)
#endif

#if MIN_VERSION_base(4,10,0)
instance FContravariant (K1 i a) where
  fcontramap _ (K1 a) = K1 a


instance FContravariant U1 where
  fcontramap _ U1 = U1

instance FContravariant V1 where
#ifndef HLINT
  fcontramap _ = \case
#endif

instance (Functor f, FContravariant g) => FContravariant (f :.: g) where
  fcontramap f = Comp1 #. fmap (fcontramap f) .# unComp1

instance (FContravariant f, FContravariant g) => FContravariant (f :*: g) where
  fcontramap f (g :*: h) = fcontramap f g :*: fcontramap f h

instance (FContravariant f, FContravariant g) => FContravariant (f :+: g) where
  fcontramap f (L1 g) = L1 (fcontramap f g)
  fcontramap f (R1 h) = R1 (fcontramap f h)
#endif

-- * Distributive Utilities

newtype Logarithm f = Logarithm { runLogarithm :: forall a. f a -> a }

indexLogarithm :: f a -> Logarithm f -> a
indexLogarithm fa (Logarithm fa2a) = fa2a fa

instance FContravariant Logarithm where
  fcontramap f g = Logarithm (runLogarithm g . f)

newtype Tab a f = Tab { runTab :: Logarithm f -> a }

instance FFunctor (Tab a) where
  ffmap f g = Tab (runTab g . fcontramap f)

-- * Elements

newtype Element a f = Element { runElement :: f a }

instance FFunctor (Element a) where
  ffmap f (Element fa) = Element (f fa)

instance FFoldable (Element a) where
  ffoldMap f (Element fa) = f fa
  flengthAcc acc _ = acc + 1

instance FTraversable (Element a) where
  ftraverse f (Element g) = Element <$> f g

-- * "natural" transformations via parametricity

newtype NT f g = NT (f ~> g)

instance FFunctor (NT f) where
  ffmap f (NT g) = NT (f . g)

data Some m where
  Some :: m a -> Some m

instance Applicative m => Semigroup (Some m) where
  Some m <> Some n = Some (m *> n)

instance Applicative m => Monoid (Some m) where
  mempty = Some (pure ())
  mappend = (<>)

instance FFunctor Some where
  ffmap f (Some m) = Some (f m)

instance FFoldable Some where
  ffoldMap f (Some m) = f m
  flengthAcc len _ = len + 1

instance FTraversable Some where
  ftraverse f (Some m) = Some <$> f m

newtype Lim f = Lim { runLim :: forall a. f a }

instance FFunctor Lim where
  ffmap f (Lim g) = Lim (f g)

instance FFoldable Lim where
  ffoldMap f (Lim g) = f g
  flengthAcc len _ = len + 1