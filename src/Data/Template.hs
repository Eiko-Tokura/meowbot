{-# LANGUAGE TemplateHaskell, AllowAmbiguousTypes #-}
module Data.Template where

import Language.Haskell.TH
import Data.Typeable

showQ :: Q Exp -> Q Exp
showQ x = [| show $x |]

patShowQ :: Show a => a -> Q Pat
patShowQ x = return $ LitP $ StringL $ show x

patShowTypeRepQ :: forall a. Typeable a => Q Pat
patShowTypeRepQ = return $ LitP $ StringL $ show $ typeRep (Proxy @a)

