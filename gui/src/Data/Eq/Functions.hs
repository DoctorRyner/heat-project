{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Eq.Functions where

instance Eq (a -> b) where
    _ == _ = True