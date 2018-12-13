{-# language ExplicitForAll #-}
module Data.VariantT.Lens (module Data.VariantT, _CtorT) where

import Control.Lens.Prism (Prism', prism')

import Data.VariantT

{-# inline _CtorT #-}
_CtorT :: forall g vs f a. CtorT vs g => Prism' (VariantT vs f a) (g f a)
_CtorT = prism' injT prjT
