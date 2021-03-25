{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Part2 where

type family Sum a b
type instance Sum Z b = b
type instance S
um (S a) b = S (Sum a b)
