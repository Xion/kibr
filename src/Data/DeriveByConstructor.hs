module Data.DeriveByConstructor where

import Prelude

import Language.Haskell.TH

deriveByConstructor :: Name -> [Name] -> Q [Dec]
deriveByConstructor t ds =
  do
    TyConI (DataD _ _ _ cs _) <- reify t
    return [ DataD [] typeName [] [NormalC (mkCon c) [] | c <- cs] ds
           , SigD funcName (AppT (AppT ArrowT (ConT t)) (ConT typeName))
           , FunD funcName
               [ Clause [RecP (getName c) []]
                        (NormalB (ConE (mkCon c))) [] | c <- cs ]
           ]
  where
    mkCon                    = mkName . ("Is" ++) . nameBase . getName
    getName (NormalC name _) = name
    getName (RecC name _)    = name
    getName _                = error "deriveByConstructor used on non-record"
    typeName                 = mkName . ("By" ++) . nameBase $ t
    funcName                 = mkName . ("by" ++) . nameBase $ t
