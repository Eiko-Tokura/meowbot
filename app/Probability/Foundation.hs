{-# OPTIONS_GHC -funfolding-use-threshold=200 -fspecialize-aggressively -Wno-missing-signatures #-} -- inline as much as possible
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

module Probability.Foundation where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Random
import System.IO.Unsafe (unsafeInterleaveIO)

--import Data.Semigroup
import Debug.Trace (trace)

import qualified Data.Map.Strict as SM
trace' x = trace (show x) x
trace'' str x = trace (show str ++ ": " ++ show x) x

-- adjust the following to change the trace level
trace2 = const id -- trace

--data Probability = forall r. (Real r) => Probability r

type UniformFloat k = (Floating k, UniformRange k)
type Probability p = p
type PDF p a = a -> p
type Weight p a = a -> p
type LogLikelihood p a = a -> p
data SampledPDF r p a = SampledPDF {pdf :: PDF p a, sample :: r a}
type DiscreteList p a  = [(p, a)] 

--data (Ord a) => Dist a = ByFunction { pdf :: a -> Probability } | ByMap { mass :: M.Map a Probability }
--data (Ord p) => DAG = DAG { points :: [p], parent :: M.Map p [p], children :: M.Map p [p], input :: M.Map p ([a] -> Dist a) }

-- State s a = State {runState :: s -> (a, s)}, i.e. it is a wrapper for Hom s (a, s), and is a monad.
-- consider State s (State s a) = Hom s (Hom s (a, s)) = Hom (s, s) (a, s) -> Hom s (a, s) where the last map is given by s -> (s, s)
-- so it is indeed a monad.
--
-- is F a = Hom s (Either s a)    a monad?
-- F (F a) = Hom s (Either s (Hom s (Either s a))) -> Hom s (Either s (Either s a)) -> Hom s (Either s a)
-- so this is indeed a monad. But where can we use this?
-- F a can be thought as a function that runs and updates a hidden state, with a chance to return a value of type a.
-- Let's call F = Might
--
-- what about Hom (Either s a) s = (Hom s s, Hom a s)?
-- Hom (Either s (Hom (Either s a) s)) s = (Hom s s, Hom (Hom (Either s a) s) s) = (End s, Hom (End s, Hom a s) s) -> (End s, Hom (Hom a s) s)
--
-- a Might s a is a computation that can end up with extra data a, and the state is updated only if the computation succeeds.
-- this does not seem too useful, let's consider if F m e s a = Hom s (m (Either (a, s) e)) is a monad.
-- F m e s (F m e s a) = Hom s (m (Either (Hom s (m (Either (a, s) e)), s) e)) -> Hom s (m (Either (m (Either (a, s) e), s) e)) -> Hom s (m (Either (a, s) e))
--
--newtype Might s a = Might {runMight :: s -> Either s a}
--
--instance Functor (Might s) where
--  fmap f (Might g) = Might $ \s -> fmap f (g s)
--
--instance Monad (Might s) where
--  return = pure
--  Might f >>= g = Might $ \s -> case f s of
--    Left  s' -> Left s'
--    Right x  -> runMight (g x) s
--
--instance Applicative (Might s) where
--  pure x = Might $ const $ Right x
--  (<*>) = ap
--
-- for example we might have some functions depend on global state
-- readFromFile :: Might s IOErr
-- doComputation :: Might s ComputationErr
-- the varioius errors will be handled in the monad
-- 
-- f = runMight $ do
--  ioErr <- readFromFile
--  computationErr <- doComputation
--  return (ioErr, computationErr)
--
-- if no error, this f will return Left s.
-- If IOErr is present, the state in readFromFile will not be changed and doComputation will continue to run on the old state.
-- This is a monad that when an error occurs, the computation will attempt to resume, but you can use all the error messages to determine how to continue.
-- it is full of error handling.
-- it will only return Right when all computations inside have failed.
--
-- It can be useful in situations where your operations have a high chance of failure but you want to continue and rescue as much as possible.
--
-- for example you can have a computation program that updates a vector, where a lot of places can have error (array out of bound, divide by zero, NaN, etc)
-- 

type Sample a = State StdGen a

runSample :: State s a -> s -> a
runSample = evalState 

liftSIO :: Sample a -> IO a
liftSIO sampler = evalState sampler <$> initStdGen

sampleR :: (UniformRange a) => (a, a) -> State StdGen a
sampleR (x, y) = do
  gen <- get
  let (v, gen') = uniformR (x, y) gen
  put gen'
  return v

class Monad m => MonadUniform m where
  getUniformR :: (UniformRange a) => (a, a) -> m a
  getUniformRs :: (UniformRange a) => (a, a) -> m [a]

  getUniform :: (UniformRange a, Num a) => m a
  getUniform = getUniformR (0, 1)
  {-# INLINE getUniform #-}

  getUniforms :: (UniformRange a, Num a) => m [a]
  getUniforms = getUniformRs (0, 1)
  {-# INLINE getUniforms #-}

class MonadUniform m => MonadInterleave m where
  interleaveRandom :: m a -> m a      -- interleave the computation, useful for IO, usually used to achieve lazy behavior, does not split the random generator.
  interleaveSplitRandom :: m a -> m a -- interleave and split the random generator.

instance MonadUniform (State StdGen) where
  getUniformR = sampleR
  getUniformRs (x, y) = (:) <$> sampleR (x, y) <*> getUniformRs (x, y)
  {-# INLINE getUniformR #-}
  {-# INLINE getUniformRs #-}

instance MonadInterleave (State StdGen) where
  interleaveRandom = id
  interleaveSplitRandom sa = do
    (s1, s2) <- gets split
    put s2
    return $ evalState sa s1
  {-# INLINE interleaveRandom #-}
  {-# INLINE interleaveSplitRandom #-}

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadUniform m where
  getUniformR = liftIO . getStdRandom . uniformR
  getUniformRs intv = liftIO $
                      (:) <$> unsafeInterleaveIO (getUniformR intv)
                          <*> unsafeInterleaveIO (getUniformRs intv)
  {-# INLINE getUniformR #-}
  {-# INLINE getUniformRs #-}

instance MonadInterleave IO where
  interleaveRandom = unsafeInterleaveIO 
  interleaveSplitRandom = unsafeInterleaveIO 
  {-# INLINE interleaveRandom #-}
  {-# INLINE interleaveSplitRandom #-}

signS :: (MonadUniform m, Num a) => m a
signS = do
  x <- getUniformR (0, 1 :: Int)
  case x of
    0 -> return (-1)
    _ -> return 1

discreteSamplingByList :: (MonadUniform m, UniformFloat p, Ord p) => DiscreteList p a -> m a
discreteSamplingByList dlist = do
  u <- getUniformR (0, 1)
  return $ subtractUntil dlist u
  where 
    subtractUntil [(_, k)] _ = k -- the last one will be used if the sum of probabilities is less than one
    subtractUntil ((p, k):rest) u
          | p > u     = k
          | otherwise = subtractUntil rest (u-p)
    subtractUntil [] _ = error "empty list"

normalizeDistLogList :: (UniformFloat p, Ord p) => DiscreteList p a -> DiscreteList p a
normalizeDistLogList list = normalizeDistList . map (\(p, y) -> (exp(p - maximum (map fst list)), y)) $ list

normalizeDistList :: (UniformFloat p) => DiscreteList p a -> DiscreteList p a
normalizeDistList list = map (\(p, y) -> (p / sumprob list, y)) list
  where sumprob = sum . map fst 
 
uniformElemS :: (MonadUniform m) => [a] -> m a
uniformElemS list = discreteSamplingByList . map ((1::Double) / len, ) $ list
  where len = fromIntegral $ length list

discreteSamplingBoundedEnumByFunction :: forall m p a. (MonadUniform m, Bounded a, Enum a, UniformFloat p, Ord p) => (a -> Probability p) -> m a
discreteSamplingBoundedEnumByFunction func = do
  u <- getUniformR (0, 1 :: Probability p)
  return $ subtractUntil (fromEnum (minBound @a)) u
  where subtractUntil i u
          | i > fromEnum (maxBound @a) = toEnum (i-1)
          | func (toEnum i) > u        = toEnum i
          | otherwise                  = subtractUntil (i+1) (u - func (toEnum i))

rejectionSampling :: (MonadUniform r, UniformFloat p, Ord p) => SampledPDF r p a -> p -> PDF p a -> r a
rejectionSampling base@(SampledPDF basePDF baseSample) m pdf = do
  x <- baseSample
  let q = pdf x / basePDF x
  u <- getUniformR (0, m)
  if u < q then return x else rejectionSampling base m pdf

rejectionSamplingByListAndBound :: (MonadUniform r, UniformFloat k, Ord k) => [k] -> k -> PDF k k -> r k
rejectionSamplingByListAndBound seq_a = rejectionSampling helper 
  where
    helper = SampledPDF (collectUntil seq_a (0::Int) 0) (geometric_a 0 seq_a)
    collectUntil (x:xs) k sum t
      | sum + x > t = 1/ (2^k * x)
      | otherwise   = collectUntil xs (k+1) (sum+x) t
    collectUntil [] _ _ _ = error "collectUntil : empty list"
    geometric_a sum (x:xs) = do
      coin <- getUniformR (0, 1::Int)
      case coin of
        0 -> getUniformR (sum, sum+x)
        _ -> geometric_a (sum+x) xs
    geometric_a _ [] = error "geometric_a : empty list"

-- Take q, and PDF gamma, computes E(phi(x)) as W^i phi(X^i) where W^i = w^i / (sum w^i). 
-- Here this function just returns (x, w)
importanceSample1 :: (MonadUniform m, UniformFloat k) => SampledPDF m k a -> PDF k a -> m (a, k)
importanceSample1 base@(SampledPDF q sample_q) gamma = do
  x <- sample_q
  return (x, gamma x / q x)

constS :: (MonadUniform m) => a -> m a
constS = return 
{-# INLINE constS #-}

type Mu k = k
type Sigma k = k
normal :: (MonadUniform m, UniformFloat k, Ord k) => Mu k -> Sigma k -> m k
normal mu sigma = fmap ((+mu).(*sigma)) $ liftM2 (*) signS $ rejectionSamplingByListAndBound seq_a bound (\t -> exp(- (t^(2::Int) / 2)) * sqrt(2 / pi)) 
  where
    seq_a = [0.62 / sqrt(fromIntegral n) | n <- [(1::Int)..]]
    bound = 4.3

normalPlus mu sigma = do
  x <- normal mu sigma
  if x <= 0 && mu > 0 then normalPlus mu sigma else return x

normalBound (a, b) mu sigma = 
  do
    x <- normal mu sigma
    if x <= a || x >= b then normalBound (a, b) mu sigma else return x

exponential :: (MonadUniform m, UniformFloat p, Ord p) => p -> m p
exponential lambda = (/lambda) <$> rejectionSamplingByListAndBound (repeat (log 2)) (2/log 2) (\t -> exp(-t))

binom :: (Integral int, MonadUniform m, UniformFloat p, Ord p) => int -> Probability p -> m int
binom n p = discreteSamplingByList (zipWith ($) [\b -> (fromIntegral b * p^k * (1-p)^(fromIntegral n-k), k) | k <- [0..n]] $ binomList !! fromIntegral n)

geometry :: (MonadUniform m, UniformFloat p, Ord p) => p -> m Integer
geometry p = discreteSamplingByList [(p*(1-p)^(k-1), k) | k <- [1..]]

poisson :: (MonadUniform m, UniformFloat p, Ord p) => p -> m Integer
poisson lambda = discreteSamplingByList poissonList
  where poissonList = (\(lp, k) -> (exp lp, k)) <$> iterate nextTerm (-lambda, 0)
        nextTerm (p, k) = (p + log lambda - log (fromIntegral k+1), k+1)

gammaS :: (MonadUniform m, UniformFloat p, Ord p) => p -> p -> m p
gammaS alpha beta
  | beta /= 1 = do
      g' <- gammaS alpha 1
      return (g'/beta)
  | alpha > 1 = do
      x <- normal 0 1
      let d = alpha - 1/3
          c = 1/sqrt(9*d)
          v = (1+c*x)^3
      if v <= 0 then
        gammaS alpha beta
      else do 
          u <- getUniformR (0, 1)
          if u < 1 - 0.0331 * x^(4::Int) then 
            return $ d*v
          else
            if log(u) < 0.5*x^(2::Int) + d*(1-v+(log v)) then 
              return $ d*v
            else
              gammaS alpha beta
  | otherwise = do
      g' <- gammaS (alpha+1) 1
      u <- getUniformR (0, 1)
      return $ g' * u**(1/alpha)

betaS :: (Ord p, MonadUniform m, UniformFloat p) => p -> p -> m p
betaS a b = do
    x <- gammaS a 1
    y <- gammaS b 1
    return (x / (x + y))

-- must be used with lists containing >= 2 elements
betaMultiS :: (MonadUniform m, UniformFloat p, Ord p) => [p] -> m [p]
betaMultiS [x, y] = do
  x' <- betaS x y
  return [x', 1-x']
betaMultiS (x:xs) = do
  x' <- betaS x (sum xs)
  (x' :) . map ((1-x')*) <$> betaMultiS xs
betaMultiS [] = error "betaMultiS : empty list"

binomList :: [[Int]]
binomList = iterate nextRow [1]
  where
    nextRow row = zipWith (+) (0 : row) (row ++ [0])

binomMapBound :: Int
binomMapBound = 20 

binomMap :: SM.Map (Int, Int) Int
binomMap = SM.fromList [((n, r), binomList !! n !! r ) | n <- [0..binomMapBound], r <- [0..n]]

var [] = 0
var list = sumxsq / (n-1) - sumx^2 / (n*(n-1))
  where sumxsq = sum $ map (^2) list
        sumx = sum list 
        n = fromIntegral $ length list

logistic :: (Floating p) => p -> p
logistic x =  log(x/(1-x)) --if x <= 0 || x >= 1 then -9999 else log(x/(1-x))

dlogistic :: Fractional a => a -> a 
dlogistic x = 1/x + 1/(1-x)

invLogistic :: Floating a => a -> a 
invLogistic x = 1/(1+exp(-x))

logLikelihoodLogisticInBound :: (Floating p, Eq p) => p -> p -> p -> p
logLikelihoodLogisticInBound 0 b 0 = 0
logLikelihoodLogisticInBound a 1 1 = 0
logLikelihoodLogisticInBound a b 0 = -999
logLikelihoodLogisticInBound a b 1 = -999
logLikelihoodLogisticInBound a b x = -((logistic x - m)^2/s^2)
  where m = (la + lb)/2
        s = (lb - la)/3.92
        la = logistic a
        lb = logistic b

likelihoodLogisticInBound :: (Floating p, Eq p) => p -> p -> p -> p
likelihoodLogisticInBound 0 b 0 = 1
likelihoodLogisticInBound a 1 1 = 1
likelihoodLogisticInBound a b 0 = 0
likelihoodLogisticInBound a b 1 = 0
likelihoodLogisticInBound a b x = exp(-((logistic x - m)^2/s^2))
  where m = (la + lb)/2
        s = (lb - la)/3.92
        la = logistic a
        lb = logistic b

logisticNormal :: (MonadUniform m, UniformFloat p, Ord p) => p -> p -> m p
logisticNormal mean variance = 
  if mean == 0 || variance == 0 then return mean
  else do
    let lx = logistic mean
    lxS <- normal 0 variance
    return $ restrict $ invLogistic (lx + min 10 (dlogistic mean) * lxS)
    where restrict = max 1e-5
