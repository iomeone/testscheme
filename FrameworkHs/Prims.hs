
module FrameworkHs.Prims
  ( UVar (..)
  , FVar (..)
  , Label (..)
  , Reg (..), numRegisters, allRegisters
  , Relop (..)
  , Binop (..)
  , Disp (..)
  , Ind (..)
  , LooseEq(..)
  , PredPrim(..), EffectPrim (..), ValPrim(..)
  , valPrimArity, effectPrimArity, predPrimArity
  , Immediate(..)
  , Datum(..)    
  )
  where

import Prelude hiding (LT, EQ, GT)
import Data.Int
import Data.Symbol

--------------------------------------------------------------------------------
-- Terminal Type Definitions ---------------------------------------------------

data UVar = UV String Integer deriving (Read, Show, Eq, Ord)
data FVar = FV Integer        deriving (Read, Show, Eq, Ord)
data Label = L String Integer deriving (Read, Show, Eq, Ord)

data UVar' = UV' Symbol Integer deriving (Show, Eq, Ord)
data Label' = L' Symbol Integer deriving (Show, Eq, Ord)

data Reg = RAX | RCX | RDX | RBX | RBP | RSI | RDI | R8 | R9
         | R10 | R11 | R12 | R13 | R14 | R15
         deriving (Read,Show,Eq,Ord, Bounded, Enum)

allRegisters :: [Reg]
allRegisters = [minBound .. maxBound]

numRegisters :: Int
numRegisters = 1 + fromEnum (maxBound :: Reg) - fromEnum (minBound :: Reg)

data Relop = LT | LTE | EQ | GT | GTE               deriving (Read,Show,Eq,Ord)
data Binop = MUL | ADD | SUB | LOGAND | LOGOR | SRA deriving (Read,Show,Eq,Ord)

data Disp = D Reg Integer deriving (Read,Show,Eq, Ord)
data Ind = I Reg Reg deriving (Read,Show,Eq, Ord)


-- High-level Scheme primitives:
----------------------------------------

data PredPrim = Lt | Lte | Eq | Gte | Gt
              | BooleanP | EqP | FixnumP | NullP | PairP | VectorP | ProcedureP 
     deriving (Read, Show, Eq, Ord)
              
data EffectPrim = SetCar | SetCdr | VectorSet | ProcedureSet
     deriving (Read, Show, Eq, Ord)
              
data ValPrim = Times | Plus | Minus | Car | Cdr | Cons
             | MakeVector | VectorLength | VectorRef | Void
             | MakeProcedure | ProcedureCode | ProcedureRef
     deriving (Read, Show, Eq, Ord)
              
valPrimArity :: ValPrim -> Int
valPrimArity vp =
  case vp of
    Times -> 2 ; Plus -> 2 ; Minus -> 2 ; Car -> 1 ; Cdr -> 1 ; Cons -> 2
    MakeVector -> 1 ; VectorLength -> 1 ; VectorRef -> 2 ; Void -> 0 ; MakeProcedure -> 2
    ProcedureCode -> 1 ; ProcedureRef -> 2
    
predPrimArity :: PredPrim -> Int
predPrimArity pp =
  case pp of
    Lt -> 2 ; Lte -> 2 ; Eq -> 2 ; Gte -> 2 ; Gt -> 2
    BooleanP -> 1 ; EqP -> 2 ; FixnumP -> 1 ; NullP -> 1 ; PairP -> 1 ; VectorP -> 1
    ProcedureP -> 1

effectPrimArity :: EffectPrim -> Int
effectPrimArity ep =
  case ep of 
    SetCar -> 2 ; SetCdr -> 2 ; VectorSet -> 3 ; ProcedureSet -> 3

data Immediate = Fixnum Int64 | NullList | HashT | HashF
     deriving (Read, Show, Eq, Ord)

data Datum = PairDatum Datum Datum
           | VectorDatum [Datum]
           | ImmediateDatum Immediate
     deriving (Read, Show, Eq, Ord)              
