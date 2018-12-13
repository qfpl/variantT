{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures, PolyKinds #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language QuantifiedConstraints #-}
module Main where

import Bound.Class (Bound(..))
import Bound.Scope (Scope(..), abstract1, toScope)
import Bound.Var (Var(..))
import Control.Lens.Fold (preview)
import Control.Lens.Review ((#))
import Control.Lens.TH (makePrisms)
import Control.Monad (ap, guard)
import Data.Deriving (deriveShow1)
import Data.Functor.Classes (Show1(..), showsPrec1)
import Data.Functor.Identity (Identity)

import Data.VariantT
import Data.VariantT.Lens

import Text.Show (showParen, showString)

data FixT f a
  = Done a
  | More (f (FixT f) a)
makePrisms ''FixT
instance (forall x. Show1 x => Show1 (f x)) => Show1 (FixT f) where
  liftShowsPrec sp sl d (Done a) =
    showParen (d > 10) $
    showString "Done " .
    sp 11 a
  liftShowsPrec sp sl d (More a) =
    showParen (d > 10) $
    showString "More " .
    liftShowsPrec sp sl 11 a
instance (Show a, forall x. Show1 x => Show1 (f x)) => Show (FixT f a) where
  showsPrec = showsPrec1
deriving instance (forall x. Functor x => Functor (f x)) => Functor (FixT f)
instance
  ( forall x. Functor x => Functor (f x)
  , Bound f
  ) => Applicative (FixT f) where; pure = return; (<*>) = ap
instance
  ( forall x. Functor x => Functor (f x)
  , Bound f
  ) => Monad (FixT f) where
  return = Done
  Done a >>= f = f a
  More a >>= f = More (a >>>= f)


data IntT (f :: k -> *) (a :: k) = IntT Int
deriveShow1 ''IntT
deriving instance Functor f => Functor (IntT f)
instance Bound IntT where
  IntT n >>>= _ = IntT n
instance (Show a, Show1 f) => Show (IntT f a) where; showsPrec = showsPrec1

pattern Int :: CtorT vs IntT => Int -> FixT (VariantT vs) a
pattern Int n <- (preview (_More._CtorT) -> Just (IntT n))
  where
    Int n = More (_CtorT # IntT n)


data AddT f a = AddT (f a) (f a)
deriveShow1 ''AddT
deriving instance Functor f => Functor (AddT f)
instance Bound AddT where
  AddT a b >>>= f = AddT (a >>= f) (b >>= f)
instance (Show a, Show1 f) => Show (AddT f a) where; showsPrec = showsPrec1

pattern Add
  :: CtorT vs AddT
  => FixT (VariantT vs) a
  -> FixT (VariantT vs) a
  -> FixT (VariantT vs) a
pattern Add a b <- (preview (_More._CtorT) -> Just (AddT a b))
  where
    Add a b = More (_CtorT # AddT a b)


data ExprT f a
  = LamT (Scope () f a)
  | AppT (f a) (f a)
deriveShow1 ''ExprT
deriving instance Functor f => Functor (ExprT f)
instance Bound ExprT where
  LamT a >>>= f = LamT (a >>>= f)
  AppT a b >>>= f = AppT (a >>= f) (b >>= f)
instance (Show a, Show1 f) => Show (ExprT f a) where; showsPrec = showsPrec1

pattern Lam
  :: CtorT vs ExprT
  => Scope () (FixT (VariantT vs)) a
  -> FixT (VariantT vs) a
pattern Lam a <- (preview (_More._CtorT) -> Just (LamT a))
  where
    Lam a = More (_CtorT # LamT a)

pattern App
  :: CtorT vs ExprT
  => FixT (VariantT vs) a
  -> FixT (VariantT vs) a
  -> FixT (VariantT vs) a
pattern App a b <- (preview (_More._CtorT) -> Just (AppT a b))
  where
    App a b = More (_CtorT # AppT a b)


test :: FixT (VariantT '[AddT, ExprT]) String
test = Lam $ abstract1 "a" (Add (Done "a") (Done "b"))

main :: IO ()
main = print test
