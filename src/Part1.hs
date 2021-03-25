{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Part1 where

import Numeric.Natural

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"

data Z
data S a

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a

class IsUnit a where
  isUnit :: Bool

instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False

guardUnit :: forall a. IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
  True  -> Left "unit is not allowed"
  False -> Right x

type family Sum a b where
  Sum Z     b = b
  Sum (S a) b = S (Sum a b)

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a]   = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten x = x

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten x = flatten (concat x)
