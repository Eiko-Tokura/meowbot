{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes, QuasiQuotes #-}
-- | Template Haskell utilities.
module Utils.TH where

import Language.Haskell.TH

-- | Given some String names, define some newtypes with those names wrapping Int with deriving clauses.
-- Each newtype will derive (Eq, Show, Ord, Read, Num) newtype.
makeNewInts :: [String] -> Q [Dec]
makeNewInts names = concat <$> mapM makeNewInt names

makeNewInt :: String -> Q [Dec]
makeNewInt s = do
  let tName   = mkName s                    -- type name (and ctor name)
      fName   = mkName ("un" <> s)          -- field accessor: un<Name>
      intT    = ConT ''Int
      bang    = Bang NoSourceUnpackedness NoSourceStrictness
      con     = RecC tName [(fName, bang, intT)]
      classes = [ConT ''Eq, ConT ''Show, ConT ''Ord, ConT ''Read, ConT ''Num, ConT ''Real, ConT ''Enum, ConT ''Integral]
      deriv   = [DerivClause (Just NewtypeStrategy) classes]
  pure [ NewtypeD [] tName [] Nothing con deriv ]
