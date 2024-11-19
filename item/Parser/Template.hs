{-# LANGUAGE TemplateHaskell #-}
module Parser.Template where

-- import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Monad.Item
import Control.Applicative

stringQ :: String -> Q Exp
stringQ []     = [| return [] |]
stringQ (c:cs) = [| just c <:> $(stringQ cs) |]

stringQ_ :: String -> Q Exp
stringQ_ []     = [| return () |]
stringQ_ (c:cs) = [| just c >> $(stringQ_ cs) |]

elemQ :: [Char] -> Q Exp
elemQ []     = [| const False |]
elemQ (c:cs) = [| \c' -> c == c' || $(elemQ cs) c' |]

itemInQ :: [Char] -> Q Exp
itemInQ cs = [| satisfy $(elemQ cs) |]

itemNotInQ :: [Char] -> Q Exp
itemNotInQ cs = [| satisfy (not . $(elemQ cs)) |]

asumE :: (Alternative f) => [f a] -> f a
asumE = asum
{-# INLINE[1] asumE #-}
{-# RULES "expand asumE" forall x.    asumE [x] = x #-}
{-# RULES "expand asumE" forall x xs. asumE (x:xs) = x <|> asumE xs #-}

