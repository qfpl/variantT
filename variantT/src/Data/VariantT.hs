{-# language KindSignatures, DataKinds, PolyKinds #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
module Data.VariantT
  ( VariantT
  , absurdVT
  , widenVT
  , elimVT
  , CtorT
  , injT
  , prjT
  )
where

import Data.VariantT.Internal
