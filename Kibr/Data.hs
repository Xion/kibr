module Kibr.Data where


import Data.Map
import Data.Set


data Dictionary
    = Dictionary { words :: Set Word }
      deriving (Eq, Show)


data Word
    = Word
        { word        :: String
        , shape       :: Shape
        , definitions :: Map Language Definition
        }
      deriving (Eq, Show, Ord)


data Language
    = Lojban
    | English
      deriving (Eq, Show, Ord)


data Definition
    = Definition
        { definition :: String
        , notes      :: Maybe String
        }
      deriving (Eq, Show, Ord)


data Shape
    = Particle
        { affixes      :: [String]
        , experimental :: Bool
        , grammar      :: Grammar
        }
    | Root
        { affixes      :: [String]
        , experimental :: Bool
        }
    | Compound
    | Loan
    | Name
    | Cluster
      deriving (Eq, Show, Ord)


data Grammar
    = A
    | BAhE
    | BAI
    | BE
    | BEhO
    | BEI
    | BIhE
    | BIhI
    | BO
    | BOI
    | BU
    | BY1
    | BY2
    | CAhA
    | CAI
    | CEhE
    | CEI
    | CO
    | COI
    | CU
    | CUhE
    | DAhO
    | DOhU
    | DOI
    | FA
    | FAhA1
    | FAhA2
    | FAhA3
    | FAhA4
    | FAhO
    | FEhE
    | FEhU
    | FIhO
    | FOI
    | FUhA
    | FUhE
    | FUhO
    | GA
    | GAhO
    | GEhU
    | GI
    | GIhA
    | GOhA
    | GOI
    | GUhA
    | I
    | JA
    | JAI
    | JOhI
    | JOI
    | KE
    | KEhE
    | KEI
    | KI
    | KOhA1
    | KOhA2
    | KOhA3
    | KOhA4
    | KOhA5
    | KOhA6
    | KOhA7
    | KOhA8
    | KU
    | KUhE
    | KUhO
    | LA
    | LAhE
    | LAU
    | LE
    | LEhU
    | LI
    | LIhU
    | LOhO
    | LOhU
    | LU
    | LUhU
    | MAhO
    | MAI
    | ME
    | MEhU
    | MOhE
    | MOhI
    | MOI
    | NA
    | NAhE
    | NAhU
    | NAI
    | NIhE
    | NIhO
    | NOI
    | NU
    | NU1
    | NUhA
    | NUhI
    | NUhU
    | PA1
    | PA2
    | PA3
    | PA4
    | PA5
    | PEhE
    | PEhO
    | PU
    | RAhO
    | ROI
    | SA
    | SE
    | SEhU
    | SEI
    | SI
    | SOI
    | SU
    | TAhE
    | TEhU
    | TEI
    | TO
    | TOI
    | TUhE
    | TUhU
    | UI1
    | UI2
    | UI3
    | UI3a
    | UI3b
    | UI3c
    | UI4
    | UI5
    | UI6
    | UI7
    | VA
    | VAU
    | VEhA
    | VEhO
    | VEI
    | VIhA
    | VUhO
    | VUhU0
    | VUhU1
    | VUhU2
    | VUhU3
    | VUhU4
    | XI
    | Y
    | ZAhO
    | ZEhA
    | ZEI
    | ZI
    | ZIhE
    | ZO
    | ZOhU
    | ZOI
    | Undefined
      deriving (Eq, Show, Read, Enum, Bounded, Ord)
