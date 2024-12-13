{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Data.HList where

import Data.Kind

-- compared to ordinary algebraic data type,
-- generalized algebraic data type (GADTs) allows you to put constraints on its various constructors (which you can't do with data), specify types on its return values

data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  (:**) :: f t -> FList f ts -> FList f (t : ts)
infixr 5 :**

getEF :: Elem e l -> FList f l -> f e
getEF  EZ    (x :** _)  = x
getEF (ES n) (_ :** xs) = getEF n xs
{-# INLINE getEF #-}

data HList ts where
  Nil  :: HList '[]
  (:*) :: t -> HList ts -> HList (t : ts)
infixr 5 :*

data Elem e l where -- a type enriched inductive natural number that holds the proof of the existence of an element in a list
  EZ :: Elem x (x : xs)
  ES :: Elem x ys -> Elem x (y : ys)

--(-->) h e = get e (extract h)

(!:) :: HList l -> Elem e l -> e
(!:) = flip getE
{-# INLINE (!:) #-}
infixl 9 !:

getE :: Elem e l -> HList l -> e
getE  EZ    (x :* _)  = x
getE (ES n) (_ :* xs) = getE n xs
{-# INLINE getE #-}

hlist = True :* (100 :: Int) :* "hello owo" :* Nil

e0 = EZ
e1 = ES EZ
e2 = ES (ES EZ)

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  (a:as) ++ bs = a : (as ++ bs)
  '[] ++ bs = bs

data Sub (ys :: [Type]) (xs :: [Type]) where -- another type enriched inductive natural number, the size of the natural number is the size of the sublist
  SZ :: Sub '[] xs
  SS :: Elem y xs -> Sub ys xs -> Sub (y:ys) xs -- note that a Sub list can be reordered or even repeating
infixr 5 `SS`

h0 = getE e0 hlist
h1 = getE e1 hlist
h2 = getE e2 hlist

getSub :: Sub ys xs -> HList xs -> HList ys
getSub SZ _ = Nil
getSub (SS e t) xs = getE e xs :* getSub t xs

-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require 
sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys) 
sub12 = e1 `SS` e2 `SS` SZ

--class Extract (es :: [Type]) (ts :: [Type]) where
--  extract :: HList ts -> HList es
--
--instance Extract '[] ts where extract _ = Nil
--instance Extract xs ts => Extract (x:xs) (x:ts) where
--  extract (x :* xs) = x :* extract xs
--instance (Extract es ts) => Extract es (t : ts) where
--  extract (x :* xs) = extract xs
--instance Extract (x:xs) ts => Extract xs ts where
--  extract (_ :* xs) = extract xs

class In e (ts :: [Type]) where 
  get  :: HList ts -> e

  getF :: FList f ts -> f e

  modifyF :: (f e -> f e) -> FList f ts -> FList f ts

instance In e (e : ts) where 
  get h = h !: EZ
  {-# INLINE get #-}
  getF h = getEF EZ h
  {-# INLINE getF #-}
  modifyF g (x :** xs) = g x :** xs
  {-# INLINE modifyF #-}

instance {-# OVERLAPPABLE #-} In e ts => In e (t : ts) where
  get (_ :* xs) = get xs
  {-# INLINE get #-}
  getF (_ :** xs) = getF xs
  {-# INLINE getF #-}
  modifyF g (x :** xs) = x :** modifyF g xs
  {-# INLINE modifyF #-}

type family TIn e (ts :: [Type]) :: Bool where
  TIn e '[] = False
  TIn e (e ': ys) = True
  TIn e (_ ': ys) = TIn e ys

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If True t f = t
  If False t f = f

hAdd :: HList as -> HList bs -> HList (as ++ bs)
hAdd Nil bs = bs
hAdd (x :* xs) bs = x :* hAdd xs bs

use :: x `In` ts => HList ts -> (x -> HList ts -> y) -> y 
use h func = func (get h) h
