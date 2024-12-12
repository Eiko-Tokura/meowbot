{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Data.HList where

import Data.Kind
import GHC.TypeLits

-- compared to ordinary algebraic data type,
-- generalized algebraic data type (GADTs) allows you to put constraints on its various constructors (which you can't do with data), specify types on its return values

data HList (ts :: k) where
  Nil  :: HList '[]
  (:+) :: t -> HList ts -> HList (t : ts)
infixr 5 :+

data Elem (e :: k) (l :: [k]) where -- a type enriched inductive natural number
  EZ :: Elem x (x : xs)
  ES :: Elem x ys -> Elem x (y : ys)

get :: Elem e l -> HList l -> e
get  EZ    (x :+ _)  = x
get (ES n) (_ :+ xs) = get n xs

hlist = True :+ (100 :: Int) :+ "hello owo" :+ Nil

e0 = EZ
e1 = ES EZ
e2 = ES (ES EZ)

type family In (x :: k) (xs :: [k]) :: Bool where
  In a (a:as) = True
  In a (_:as) = In a as
  In a '[] = False

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  (a:as) ++ bs = a : (as ++ bs)
  '[] ++ bs = bs

data Sub (ys :: [k]) (xs :: [k]) where -- another type enriched inductive natural number, the size of the natural number is the size of the sublist
  SZ :: Sub '[] xs
  SS :: Elem y xs -> Sub ys xs -> Sub (y:ys) xs -- note that a Sub list can be reordered or even repeating
infixr 5 `SS`

h0 = get e0 hlist
h1 = get e1 hlist
h2 = get e2 hlist

getSub :: forall (ys :: [Type]) (xs :: [Type]). Sub ys xs -> HList xs -> HList ys
getSub SZ _ = Nil
getSub (SS e t) xs = get e xs :+ getSub t xs

-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require 
sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys) 
sub12 = e1 `SS` e2 `SS` SZ

hAdd :: HList as -> HList bs -> HList (as ++ bs)
hAdd Nil bs = bs
hAdd (x :+ xs) bs = x :+ hAdd xs bs
