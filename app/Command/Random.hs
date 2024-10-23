{-# LANGUAGE OverloadedStrings #-}
module Command.Random where

import Command
import MeowBot.BotStructure
import Probability.Foundation
import MonParserF as MP
import qualified Data.Text as T
import Control.Monad.Trans.ReaderState
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

data RandomQuery 
  = Distribution DistributionFamily
  | ChooseFromList [String]
  | CustomDistribution [(Double, String)]

data DistributionFamily
  = Uniform Double Double
  | Normal Double Double
  | Exponential Double
  | Poisson Double
  | Binomial Int Double
  | Geometric Double
  | Beta Double Double

sampleQuery :: (MonadUniform m) => RandomQuery -> m (Either String (Either Int Double))
sampleQuery (Distribution (Uniform a b)) | a < b = Right . Right <$> getUniformR (a, b)
                                         | otherwise = return $ Left "Invalid range for uniform distribution, a1 < a2 required."
sampleQuery (Distribution (Normal a b))    = Right . Right <$> normal a b
sampleQuery (Distribution (Exponential a)) = Right . Right <$> exponential a
sampleQuery (Distribution (Poisson a))     = Right . Left . fromIntegral <$> poisson a
sampleQuery (Distribution (Binomial a b))  = Right . Left  <$> binom a b
sampleQuery (Distribution (Geometric a))   = Right . Left . fromIntegral <$> geometry a
sampleQuery (Distribution (Beta a b))      = Right . Right <$> betaS a b
sampleQuery (ChooseFromList list)          = Left          <$> uniformElemS list
sampleQuery (CustomDistribution list)      = Left          <$> discreteSamplingByList (normalizeDistList list)

commandRandom :: BotCommand --ReaderStateT WholeChat OtherData IO [BotAction]
commandRandom = BotCommand Random $ botT $ do
  (msg, cid, _, _) <- MaybeT $ getEssentialContent <$> ask
  randomParser' <- lift $ commandParserTransformByBotName randomParser
  query <- MaybeT $ return $ MP.mRunParserF randomParser' msg
  do
    result <- sampleQuery query
    return [baSendToChatId cid $ T.pack $ display result]
  where
    display (Left str) = str
    display (Right (Left i)) = show i ++ " :: Int"
    display (Right (Right d)) = show d ++ " :: Double"
    randomParser :: ParserF Char RandomQuery
    randomParser = foldr1 (<>) 
      [ do
          headCommand "random"
          commandSeparator
          foldr1 (<>)
            [ string "uniform"     >> commandSeparator >> (fmap Distribution . Uniform     <$> (float <* spaces) <*> float)
            , string "normal"      >> commandSeparator >> (fmap Distribution . Normal      <$> (float <* spaces) <*> positiveFloat)
            , string "exponential" >> commandSeparator >> (     Distribution . Exponential <$> positiveFloat)
            , string "poisson"     >> commandSeparator >> (     Distribution . Poisson     <$> positiveFloat)
            , string "binomial"    >> commandSeparator >> (fmap Distribution . Binomial    <$> positiveInt <* spaces <*> positiveFloat)
            , string "geometric"   >> commandSeparator >> (     Distribution . Geometric   <$> positiveFloat)
            , string "beta"        >> commandSeparator >> (fmap Distribution . Beta        <$> (positiveFloat <* spaces) <*> positiveFloat)
            , string "choose" >> (ChooseFromList <$> many (commandSeparator >> word))
            , string "custom" >> (CustomDistribution <$> many ((,) <$> (commandSeparator >> positiveFloat) <*> (commandSeparator >> word)))
            ]
      , do
          headCommand "choose"
          ChooseFromList <$> many (commandSeparator >> word)
      ]

helpRandom :: T.Text
helpRandom = T.intercalate "\n"
  [ ":random <distribution> <parameters>"
  , ":random choose <items> / :choose <items>"
  , ":random custom p1 <item1> p2 <item2> ..."
  , ""
  , "Supported distributions:"
  , "uniform <a> <b> :: Uniform distribution in [a, b)"
  , "normal <mean> <std> :: Normal distribution with mean and standard deviation"
  , "exponential <lambda> :: Exponential distribution with rate lambda"
  , "poisson <lambda> :: Poisson distribution with rate lambda"
  , "binomial <n> <p> :: Binomial distribution with n trials and success probability p"
  , "geometric <p> :: Geometric distribution with success probability p"
  , "beta <a> <b> :: Beta distribution with parameters a and b"
  , "Example: :random uniform 0 1"
  , "Example: :choose a b c"
  , "Example: :random custom 0.1 a 0.2 b 0.3 c"
  ]
