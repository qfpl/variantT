{-# language DataKinds, KindSignatures, PolyKinds #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language InstanceSigs #-}
{-# language QuantifiedConstraints #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Data.VariantT.Internal where

import Bound.Class (Bound(..))
import Data.Functor.Classes (Show1(..), showsPrec1)
import Data.Kind (Constraint)
import GHC.Base (Any)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Unsafe.Coerce (unsafeCoerce)

data VariantT (vs :: [(k -> *) -> k -> *]) (f :: k -> *) (a :: k)
  = VariantT {-# unpack #-} !Word Any
type role VariantT representational representational representational

{-# inline elimVT #-}
elimVT
  :: forall v vs f a r
   . (v f a -> r)
  -> (VariantT vs f a -> r)
  -> VariantT (v ': vs) f a -> r
elimVT f g (VariantT tag a) =
  if tag == 0
  then f (unsafeCoerce a :: v f a)
  else g (VariantT (tag-1) a)

{-# inline widenVT #-}
widenVT :: VariantT as f x -> VariantT (a ': as) f x
widenVT (VariantT tag a) = VariantT (tag+1) a

{-# inline absurdVT #-}
absurdVT :: VariantT '[] f a -> b
absurdVT _ = error "absurdVT: absurd!"

instance Bound (VariantT '[]) where
  {-# inline (>>>=) #-}
  a >>>= _ = absurdVT a

instance (Bound v, Bound (VariantT vs)) => Bound (VariantT (v ': vs)) where
  {-# inline (>>>=) #-}
  a >>>= f = elimVT (injT . (>>>= f)) (widenVT . (>>>= f)) a

instance Functor f => Functor (VariantT '[] f) where
  {-# inline fmap #-}
  fmap _ = absurdVT

instance Foldable f => Foldable (VariantT '[] f) where
  {-# inline foldMap #-}
  foldMap _ = absurdVT

instance Traversable f => Traversable (VariantT '[] f) where
  {-# inline traverse #-}
  traverse _ = absurdVT

instance
  ( Functor f
  , forall x. Functor x => Functor (v x)
  , Functor (VariantT vs f)
  ) => Functor (VariantT (v ': vs) f) where
  {-# inline fmap #-}
  fmap f =
    elimVT
      (injT . fmap f)
      (widenVT . fmap f)

instance
  ( Foldable f
  , forall x. Foldable x => Foldable (v x)
  , Foldable (VariantT vs f)
  ) => Foldable (VariantT (v ': vs) f) where
  {-# inline foldMap #-}
  foldMap f =
    elimVT
      (foldMap f)
      (foldMap f)

instance
  ( Traversable f
  , forall x. Functor x => Functor (v x)
  , forall x. Foldable x => Foldable (v x)
  , forall x. Traversable x => Traversable (v x)
  , Traversable (VariantT vs f)
  ) => Traversable (VariantT (v ': vs) f) where
  {-# inline traverse #-}
  traverse f =
    elimVT
      (fmap injT . traverse f)
      (fmap widenVT . traverse f)

instance Show1 f => Show1 (VariantT '[] f) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec _ _ _ = absurdVT

instance
  ( Show1 f
  , forall x. Show1 x => Show1 (v x)
  , Show1 (VariantT vs f)
  ) => Show1 (VariantT (v ': vs) f) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec a b c =
    elimVT
      (\val ->
         showParen (c > 10) $
         showString "VariantT " .
         liftShowsPrec a b 11 val)
      (liftShowsPrec a b c)

instance (Show1 (VariantT vs f), Show1 f, Show a) => Show (VariantT vs f a) where
  {-# inline showsPrec #-}
  showsPrec = showsPrec1

class CtorT as g | as -> g where
  injT :: g f a -> VariantT as f a
  prjT :: VariantT as f a -> Maybe (g f a)

instance {-# overlapping #-} CtorT (v ': vs) v where
  {-# inline injT #-}
  injT = VariantT 0 . unsafeCoerce

  {-# inline prjT #-}
  prjT = elimVT Just $ const Nothing

instance {-# overlappable #-} CtorT vs b => CtorT (v ': vs) b where
  {-# inline injT #-}
  injT = widenVT . injT

  {-# inline prjT #-}
  prjT :: forall as f a g. CtorT vs g => VariantT (v ': vs) f a -> Maybe (g f a)
  prjT (VariantT tag a) = prjT (VariantT (tag-1) a :: VariantT vs f a)
