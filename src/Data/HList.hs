{-# LANGUAGE DataKinds, TypeOperators, LinearTypes, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | This module provides utilities for working with type-level lists in Haskell.
-- It defines various types and functions to manipulate type-level lists, including
-- finite lists, constrained lists, and dynamic lists.
module Data.HList where

import Data.Kind
import Data.Proxy
import Data.Default

-- | A type-level list applied to a type-level function, representing a product.
-- It has a strict head and a lazy tail.
-- the ! bang pattern here is to make it strict because it might cause trouble when putting in a stateful monad. Alternatively we can also write a strict version FList, SFList.
data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  (:**) :: !(f t) -> FList f ts -> FList f (t : ts)
infixr 5 :**

-- | A type-level list representing a simple sum of types.
data SList (ts :: [Type]) where
  SNil  :: SList '[]
  SHead :: t -> SList (t : ts)
  STail :: SList ts -> SList (t : ts)

-- | A type-level list applied to a type-level function, representing a sum.
data UList (f :: Type -> Type) (ts :: [Type]) where
  UNil  :: UList f '[]
  UHead :: f t -> UList f (t : ts)
  UTail :: UList f ts -> UList f (t : ts)

---------------------------------- Constraint Lists ----------------------------------

-- | A type-level list with constraints, allowing for the extraction of elements
-- based on certain type-level conditions.
data CList (f :: Type -> Constraint) (ts :: [Type]) where
  CNil  :: CList f '[]
  CCons :: f t => t -> CList f ts -> CList f (t : ts)
infixr 5 `CCons`

-- | A dynamic type-level list with constraints, so that you can only use the constraints
-- you know nothing else about the contents.
data CListDynamic (c :: Type -> Constraint) where
  CDNil  :: CListDynamic f
  CDCons :: c t => t -> CListDynamic c -> CListDynamic c
infixr 5 `CDCons`

-- | A constrained finite list that associates constraints with types.
data CFList (c :: k -> Constraint) (f :: k -> Type) (ts :: [k]) where
  CFNil  :: CFList c f '[]
  CFCons :: c t => f t -> CFList c f ts -> CFList c f (t : ts)
infixr 5 `CFCons`

-- | Default instance for an empty constrained finite list.
instance Default (CFList c f '[]) where
  def = CFNil
  {-# INLINE def #-}

-- | Default instance for a non-empty constrained finite list.
instance (c p, Default (CFList c Proxy ps)) => Default (CFList c Proxy (p : ps)) where
  def = CFCons Proxy def
  {-# INLINE def #-}

-- | Map a function over a constrained finite list.
cfListMap :: CFList c f ts -> (forall t. c t => f t -> a) -> [a]
cfListMap CFNil _ = []
cfListMap (CFCons t ts) f = f t : cfListMap ts f
{-# INLINE cfListMap #-}

-- | Pick an element from a constrained finite list that satisfies a predicate.
cfListPickElem :: CFList c f ts -> (forall t. c t => f t -> Bool) -> Maybe ((forall t. c t => f t -> a) -> a)
cfListPickElem CFNil _ = Nothing
cfListPickElem (CFCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cfListPickElem ts predicate
{-# INLINE cfListPickElem #-}

-- | Pick an element from a constrained list that satisfies a predicate.
cListPickElem :: CList c ts -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListPickElem CNil _ = Nothing
cListPickElem (CCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cListPickElem ts predicate
{-# INLINE cListPickElem #-}

-- | Pick an element from a dynamic constrained list that satisfies a predicate.
cListDynamicPickElem :: CListDynamic c -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListDynamicPickElem CDNil _ = Nothing
cListDynamicPickElem (CDCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cListDynamicPickElem ts predicate
{-# INLINE cListDynamicPickElem #-}

-- | Map a function over a constrained list.
cListMap :: CList c ts -> (forall t. c t => t -> a) -> [a]
cListMap CNil _ = []
cListMap (CCons t ts) f = f t : cListMap ts f
{-# INLINE cListMap #-}

-- | Map a function over a dynamic constrained list.
cListDynamicMap :: CListDynamic c -> (forall t. c t => t -> a) -> [a]
cListDynamicMap CDNil _ = []
cListDynamicMap (CDCons t ts) f = f t : cListDynamicMap ts f
{-# INLINE cListDynamicMap #-}

-- | A class for working with constraint lists, providing methods to use and pick constraints.
class ConstraintList (c :: k -> Constraint) (ts :: [k]) where
  useConstraint  :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> a) -> [a]
  pickConstraint :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> Bool) -> Maybe ((forall t. c t => Proxy t -> a) -> a)

-- | Instance for an empty constraint list.
instance ConstraintList c '[] where
  useConstraint  _ _ _ = []
  pickConstraint _ _ _ = Nothing
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

-- | Instance for a non-empty constraint list.
instance (c t, ConstraintList c ts) => ConstraintList c (t : ts) where
  useConstraint  pc _ f = f (Proxy @t) : useConstraint pc (Proxy @ts) f
  pickConstraint pc _ predicate
    | predicate (Proxy :: Proxy t) = Just $ \f -> f (Proxy @t)
    | otherwise = pickConstraint pc (Proxy @ts) predicate
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

------------------------------Classical HList----------------------------------

-- | A type-level list representing a product.
data HList ts where
  Nil  :: HList '[]
  (:*) :: t -> HList ts -> HList (t : ts)
infixr 5 :*

-- | Get an element from a finite list using an element proof.
getEF :: Elem e l -> FList f l -> f e
getEF  EZ    (x :** _)  = x
getEF (ES n) (_ :** xs) = getEF n xs
{-# INLINE getEF #-}

-- | Proof of the existence of an element in a list.
data Elem e l where
  EZ :: Elem x (x : xs)              -- ^ Base case: the element is the head of the list.
  ES :: Elem x ys -> Elem x (y : ys) -- ^ Inductive case: the element is in the tail of the list.

-- | Get the element in the HList using the proof.
(!:) :: HList l -> Elem e l -> e
(!:) = flip getE
{-# INLINE (!:) #-}
infixl 9 !:

-- | Get the element in the HList using the proof.
getE :: Elem e l -> HList l -> e
getE  EZ    (x :* _)  = x
getE (ES n) (_ :* xs) = getE n xs
{-# INLINE getE #-}

-- | Put an element into the HList at the position specified by the proof.
putE :: Elem e l -> e -> HList l -> HList l
putE  EZ    x (_ :* xs) = x :* xs
putE (ES n) x (y :* xs) = y :* putE n x xs
{-# INLINE putE #-}

-- | A type-level function that concatenates two type-level lists.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a:as) ++ bs = a : (as ++ bs)

-- | A data-level proof of the existence of a sublist in a list.
--
-- Examples
--
-- sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys)
-- sub12 = e1 `SS` e2 `SS` SZ
--
-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require
data Sub (ys :: [Type]) (xs :: [Type]) where
  SZ :: Sub '[] xs                              -- ^ Base case: the empty sublist.
  SS :: Elem y xs -> Sub ys xs -> Sub (y:ys) xs -- ^ Inductive case: the head element is in the list.

-- | Get the sublist in the HList using the proof.
getSub :: Sub ys xs -> HList xs -> HList ys
getSub SZ _        = Nil
getSub (SS e t) xs = getE e xs :* getSub t xs

-- | A class carrying the proof of the existence of an element in a list.
class In e (ts :: [Type]) where
  getH :: HList ts -> e                               -- ^ Get the element from the HList.
  getF :: FList f ts -> f e                           -- ^ Get the element from the FList.
  modifyH :: (e -> e) -> HList ts -> HList ts         -- ^ Modify the element in the HList.
  modifyF :: (f e -> f e) -> FList f ts -> FList f ts -- ^ Modify the element in the FList.
  embedU :: f e -> UList f ts                         -- ^ Embed the element into a UList.

-- | Base case for the In class.
instance In e (e : ts) where
  getH h = h !: EZ
  {-# INLINE getH #-}
  getF = getEF EZ
  {-# INLINE getF #-}
  modifyH f (x :* xs) = f x :* xs
  {-# INLINE modifyH #-}
  modifyF g (x :** xs) = g x :** xs
  {-# INLINE modifyF #-}
  embedU = UHead
  {-# INLINE embedU #-}

-- | Inductive case for the In class.
instance {-# OVERLAPPABLE #-} In e ts => In e (t : ts) where
  getH (_ :* xs) = getH xs
  {-# INLINE getH #-}
  getF (_ :** xs) = getF xs
  {-# INLINE getF #-}
  modifyH f (x :* xs) = x :* modifyH f xs
  {-# INLINE modifyH #-}
  modifyF g (x :** xs) = x :** modifyF g xs
  {-# INLINE modifyF #-}
  embedU x = UTail $ embedU x
  {-# INLINE embedU #-}

-- | Sum of two type-level lists.
hAdd :: HList as -> HList bs -> HList (as ++ bs)
hAdd Nil bs = bs
hAdd (x :* xs) bs = x :* hAdd xs bs

-- | A class for working with sublists.
class SubList (ys :: [Type]) (xs :: [Type]) where
  getSubList     :: HList xs -> HList ys                                   -- ^ Get the sublist from the HList.
  getSubListF    :: FList f xs -> FList f ys                               -- ^ Get the sublist from the FList.
  subListModify  :: (HList ys -> HList ys) -> HList xs -> HList xs         -- ^ Modify the sublist in the HList.
  subListModifyF :: (FList f ys -> FList f ys) -> FList f xs -> FList f xs -- ^ Modify the sublist in the FList.
  subListUpdate  :: HList xs -> HList ys -> HList xs                       -- ^ Update the sublist in the HList.
  subListUpdate xs ys = subListModify (const ys) xs
  {-# INLINE subListUpdate #-}

  subListUpdateF :: FList f xs -> FList f ys -> FList f xs -- ^ Update the sublist in the FList.
  subListUpdateF xs ys = subListModifyF (const ys) xs
  {-# INLINE subListUpdateF #-}

-- | A class for embedding sublists.
class EmbedSubList (ys :: [Type]) (xs :: [Type]) where
  embedSubList :: UList f ys -> UList f xs -- ^ Embed a UList of ys into a UList of xs.

instance EmbedSubList '[] '[] where
  embedSubList = id
  {-# INLINE embedSubList #-}

instance EmbedSubList '[] xs => EmbedSubList '[] (x : xs) where
  embedSubList _ = UTail $ embedSubList @'[] @xs UNil
  {-# INLINE embedSubList #-}

instance (EmbedSubList ys xs, In y xs) => EmbedSubList (y : ys) xs where
  embedSubList (UHead y)  = embedU y
  embedSubList (UTail ys) = embedSubList ys
  {-# INLINE embedSubList #-}

instance {-# OVERLAPPING #-} EmbedSubList ys (y : ys) where
  {-# INLINE embedSubList #-}
  embedSubList = UTail

instance SubList '[] xs where
  getSubList _ = Nil
  {-# INLINE getSubList #-}
  getSubListF _ = FNil
  {-# INLINE getSubListF #-}
  subListModify _ xs = xs
  {-# INLINE subListModify #-}
  subListModifyF _ xs = xs
  {-# INLINE subListModifyF #-}

-- | Induction case for the SubList class.
instance (In y xs, SubList ys xs) => SubList (y : ys) xs where
  getSubList xs = getH xs :* getSubList xs
  {-# INLINE getSubList #-}
  getSubListF xs = getF xs :** getSubListF xs
  {-# INLINE getSubListF #-}
  subListModify f xs =
    let hy :* hys = f (getSubList xs)
    in modifyH (const hy) $ subListUpdate xs hys
  {-# INLINE subListModify #-}
  subListModifyF f xs =
    let hy :** hys = f (getSubListF xs)
    in modifyF (const hy) $ subListUpdateF xs hys
  {-# INLINE subListModifyF #-}

