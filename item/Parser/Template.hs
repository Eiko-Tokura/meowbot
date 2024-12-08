{-# LANGUAGE TemplateHaskell #-}
-- | Author: Eiko chan >w<
--
-- This module provides a few Template Haskell functions that are useful for generating parsers.
module Parser.Template where

-- import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Monad.Item
import Control.Applicative

-- | Generate a sequence of justs from a list of characters.
stringQ :: String -> Q Exp
stringQ []     = [| return [] |]
stringQ (c:cs) = [| just c <:> $(stringQ cs) |]

-- | Generate a sequence of justs from a list of characters.
-- $(stringQ_ "abc") = just 'a' >> just 'b' >> just 'c' >> return ()
stringQ_ :: String -> Q Exp
stringQ_ []     = [| return () |]
stringQ_ (c:cs) = [| just c >> $(stringQ_ cs) |]

-- | Generate a sequence of equality checks from a list of characters.
-- $(elemQ "abc") = \c -> c == 'a' || c == 'b' || c == 'c'
elemQ :: [Char] -> Q Exp
elemQ []     = [| const False |]
elemQ (c:cs) = [| \c' -> c == c' || $(elemQ cs) c' |]

-- | Generate a parser that parse an item in a list of characters, expands to sequence of equality checks.
-- $(itemInQ "abc") = satisfy (\c -> c == 'a' || c == 'b' || c == 'c')
itemInQ :: [Char] -> Q Exp
itemInQ cs = [| satisfy $(elemQ cs) |]

-- | Generate a parser that parse an item not in a list of characters, expands to sequence of equality checks.
-- $(itemNotInQ "abc") = satisfy (\c -> not (c == 'a' || c == 'b' || c == 'c'))
itemNotInQ :: [Char] -> Q Exp
itemNotInQ cs = [| satisfy (not . $(elemQ cs)) |]

-- | An `asum` that will expand to a series of <|> operators during compilation. Use it on constant or literal lists.
-- asumE [x, y, z] will expand to x <|> y <|> z, eliminating the list overhead.
asumE :: (Alternative f) => [f a] -> f a
asumE = asum
{-# INLINE[1] asumE #-}
{-# RULES "expand asumE" forall x.    asumE [x] = x #-}
{-# RULES "expand asumE" forall x xs. asumE (x:xs) = x <|> asumE xs #-}
