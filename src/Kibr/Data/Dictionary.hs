{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Dictionary where

import Preamble

import Kibr.Data.Grammar
import Kibr.Data.Language
import Kibr.Data.Revision

import Data.Data
import Data.Lens.Template
import Data.Map
import Data.SafeCopy
import Data.Set

data Shape
  = Particle
      { _affixes      :: Set String
      , _experimental :: Bool
      , _grammar      :: Grammar
      }
  | Root
      { _affixes      :: Set String
      , _experimental :: Bool
      }
  | Compound
  | Loan
  | Name
  | Cluster
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Shape
makeLens ''Shape

data Definition
  = Definition
      { _definition :: String
      , _notes      :: Maybe String
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Definition
makeLens ''Definition

data Word
  = Word
      { _word        :: String
      , _shape       :: Shape
      , _definitions :: Map Language [Revision Definition]
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Word
makeLens ''Word

data Dictionary
  = Dictionary { _words :: Set Word }
    deriving (Eq, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Dictionary
makeLens ''Dictionary

empty :: Dictionary
empty = Dictionary Data.Set.empty