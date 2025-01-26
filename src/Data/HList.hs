{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Some utilities for working with type-level lists
module Data.HList where

import Data.Kind
import Data.Proxy

-- compared to ordinary algebraic data type,
-- generalized algebraic data type (GADTs) allows you to put constraints on its various constructors (which you can't do with data), specify types on its return values

-- | A type-level list applied to a type-level function, product
data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  (:**) :: !(f t) -> FList f ts -> FList f (t : ts)
infixr 5 :**

-- the ! bang pattern here is to make it strict because it might cause trouble when putting in a stateful monad. Alternatively we can also write a strict version FList, SFList.

-- | A type-level list applied to a type-level function, sum
data UList (f :: Type -> Type) (ts :: [Type]) where
  UNil  :: UList f '[]
  UHead :: f t -> UList f (t : ts)
  UTail :: UList f ts -> UList f (t : ts)
---------------------------------- Constraint Lists ----------------------------------

-- | A type-level list, with constraints
data CList (f :: Type -> Constraint) (ts :: [Type]) where
  CNil  :: CList f '[]
  CCons :: f t => t -> CList f ts -> CList f (t : ts)

data CListDynamic (f :: Type -> Constraint) where
  CDNil  :: CListDynamic f
  CDCons :: f t => t -> CListDynamic f -> CListDynamic f

cListPickElem :: CList c ts -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListPickElem CNil _ = Nothing
cListPickElem (CCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise = cListPickElem ts predicate
{-# INLINE cListPickElem #-}

cListDynamicPickElem :: CListDynamic c -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListDynamicPickElem CDNil _ = Nothing
cListDynamicPickElem (CDCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise = cListDynamicPickElem ts predicate
{-# INLINE cListDynamicPickElem #-}

cListMap :: CList c ts -> (forall t. c t => t -> a) -> [a]
cListMap CNil _ = []
cListMap (CCons t ts) f = f t : cListMap ts f
{-# INLINE cListMap #-}

cListDynamicMap :: CListDynamic c -> (forall t. c t => t -> a) -> [a]
cListDynamicMap CDNil _ = []
cListDynamicMap (CDCons t ts) f = f t : cListDynamicMap ts f
{-# INLINE cListDynamicMap #-}

class ConstraintList (c :: k -> Constraint) (ts :: [k]) where
  useConstraint  :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> a) -> [a]
  pickConstraint :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> Bool) -> Maybe ((forall t. c t => Proxy t -> a) -> a)

instance ConstraintList c '[] where
  useConstraint _ _ _ = []
  pickConstraint _ _ _ = Nothing
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

instance (c t, ConstraintList c ts) => ConstraintList c (t : ts) where
  useConstraint pc _ f = f (Proxy @t) : useConstraint pc (Proxy @ts) f
  pickConstraint pc _ predicate
    | predicate (Proxy :: Proxy t) = Just $ \f -> f (Proxy @t)
    | otherwise = pickConstraint pc (Proxy @ts) predicate
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

-- | A type-level list, product
data HList ts where
  Nil  :: HList '[]
  (:*) :: t -> HList ts -> HList (t : ts)
infixr 5 :*

-- | Using an element proof to get the element in the flist
getEF :: Elem e l -> FList f l -> f e
getEF  EZ    (x :** _)  = x
getEF (ES n) (_ :** xs) = getEF n xs
{-# INLINE getEF #-}

-- | proof of the existence of an element in a list
data Elem e l where -- a type enriched inductive natural number that holds the proof of the existence of an element in a list
  EZ :: Elem x (x : xs)
  ES :: Elem x ys -> Elem x (y : ys)

-- | get the element in the hlist using the proof
(!:) :: HList l -> Elem e l -> e
(!:) = flip getE
{-# INLINE (!:) #-}
infixl 9 !:

-- | get the element in the hlist using the proof
getE :: Elem e l -> HList l -> e
getE  EZ    (x :* _)  = x
getE (ES n) (_ :* xs) = getE n xs
{-# INLINE getE #-}

-- examples
-- hlist = True :* (100 :: Int) :* "hello owo" :* Nil
--
-- e0 = EZ
-- e1 = ES EZ
-- e2 = ES (ES EZ)
--
-- h0 = getE e0 hlist
-- h1 = getE e1 hlist
-- h2 = getE e2 hlist

-- | a type level function that concatenates two type level lists
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  (a:as) ++ bs = a : (as ++ bs)
  '[] ++ bs = bs

-- | A proof of the existence of a sublist in a list
data Sub (ys :: [Type]) (xs :: [Type]) where -- another type enriched inductive natural number, the size of the natural number is the size of the sublist
  SZ :: Sub '[] xs
  SS :: Elem y xs -> Sub ys xs -> Sub (y:ys) xs -- note that a Sub list can be reordered or even repeating
infixr 5 `SS`

-- | get the sublist in the hlist using the proof
getSub :: Sub ys xs -> HList xs -> HList ys
getSub SZ _ = Nil
getSub (SS e t) xs = getE e xs :* getSub t xs

-- examples
-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require
--
-- sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys)
-- sub12 = e1 `SS` e2 `SS` SZ

-- | A class carrying the proof of the existence of an element in a list
class In e (ts :: [Type]) where
  getH :: HList ts -> e

  getF :: FList f ts -> f e

  modifyF :: (f e -> f e) -> FList f ts -> FList f ts

-- | Base case
instance In e (e : ts) where
  getH h = h !: EZ
  {-# INLINE getH #-}
  getF h = getEF EZ h
  {-# INLINE getF #-}
  modifyF g (x :** xs) = g x :** xs
  {-# INLINE modifyF #-}

-- | Inductive case
instance {-# OVERLAPPABLE #-} In e ts => In e (t : ts) where
  getH (_ :* xs) = getH xs
  {-# INLINE getH #-}
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

-- | Sum of two type-level lists
hAdd :: HList as -> HList bs -> HList (as ++ bs)
hAdd Nil bs = bs
hAdd (x :* xs) bs = x :* hAdd xs bs

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

