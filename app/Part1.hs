{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Part1 where

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"
