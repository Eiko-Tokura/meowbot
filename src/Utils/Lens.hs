module Utils.Lens where

import Control.Lens
import Language.Haskell.TH

addUnderscore :: FieldNamer
addUnderscore _ _ name = let name' = nameBase name in [TopName (mkName ('_':name'))]

makeLenses_ :: Name -> Q [Dec]
makeLenses_ = makeLensesWith $ lensRules & lensField .~ addUnderscore
