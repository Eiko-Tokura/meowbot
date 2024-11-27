{-# LANGUAGE TemplateHaskell #-}
module Data.Template where

import Language.Haskell.TH

showQ :: Q Exp -> Q Exp
showQ x = [| show $x |]

patShowQ :: Show a => a -> Q Pat
patShowQ x = return $ LitP $ StringL $ show x


