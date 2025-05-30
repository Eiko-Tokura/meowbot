{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Utils.Unit where

import GHC.TypeError
import Language.Haskell.TH

unit :: Bool -> Q Type
unit True  = [t| () |]
unit False = [t| TypeError ('Text "Unit test FAILED o.o!") |]

unitNamed :: String -> Bool -> Q Type
unitNamed _name True  = [t| () |]
unitNamed name  False = let errMsg = "Unit test " <> name <> " FAILED o.o!"
  in [t| TypeError ('Text $(pure $ LitT (StrTyLit errMsg))) |]

units :: [Bool] -> Q Type
units = go 1
  where
    go :: Int -> [Bool] -> Q Type
    go _ [] = [t| () |]
    go n (x:xs) | x = go (n + 1) xs
                | otherwise = let errMsg = "Unit test #" <> show n <> " FAILED o.o!"
                              in [t| TypeError ('Text $(pure $ LitT (StrTyLit errMsg))) |]

unitsNamed :: [(String, Bool)] -> Q Type
unitsNamed = go 1
  where
    go :: Int -> [(String, Bool)] -> Q Type
    go _ [] = [t| () |]
    go n ((name, x):xs) | x = go (n + 1) xs
                        | otherwise = let errMsg = "Unit test #" <> show n <> ": " <> name <> " FAILED o.o!"
                                      in [t| TypeError ('Text $(pure $ LitT (StrTyLit errMsg))) |]
