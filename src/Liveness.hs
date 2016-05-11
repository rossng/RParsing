module Liveness where

import qualified Data.Set as Set

type Name = String
type Label = String

data Binop = Add | Subtract | Multiply | Divide | And | Or | Lt | Lte | Eq | Gte | Gt | Neq deriving (Eq, Show)
data Unop = Negate | Not deriving (Eq, Show)
data Operand = Const Integer | Temp Name deriving (Eq, Show)

data Quadruple =
      Binary Binop Operand Operand Operand      -- x := y binop z
    | Unary Unop Operand Operand                -- x := unop y
    | Assign Operand Operand                    -- x := y
    | Lbl Label                                 -- L:
    | Goto Label                                -- goto L
    | If Label Operand                          -- if x goto L
    | Par Operand                               -- param x
    | Call Name Operand                         -- call p with x parameters
    | Fcall Operand Name Operand                -- x := call f with y parameters
    | Ret                                       -- procedure return
    | Fret Operand                              -- function return x
    | Ldar Operand Operand Operand              -- x := y[z]
    | Star Operand Operand Operand              -- x[y] := z
    deriving (Eq, Show)

type Program = [Quadruple]

program :: Program
program =
  [ Assign (Temp "b") (Const 1)
  , Assign (Temp "a") (Temp "b")
  , Assign (Temp "g") (Const 2)
  , Assign (Temp "d") (Const 3)
  , Binary Multiply (Temp "c") (Temp "g") (Temp "g")
  , Binary Add (Temp "e") (Temp "a") (Temp "d")
  , Binary Add (Temp "b") (Temp "c") (Temp "c")
  , Binary Multiply (Temp "f") (Temp "e") (Temp "e")
  , Binary Add (Temp "h") (Temp "e") (Temp "e")
  , Assign (Temp "h") (Temp "b")
  , Binary Add (Temp "h") (Temp "h") (Temp "f")
  , Assign (Temp "g") (Temp "h")
  , Par (Temp "g")
  , Call "write" (Const 1) ]

toVariableNames :: [Operand] -> [Name]
toVariableNames (o:os) = case o of
                          (Const _) -> toVariableNames os
                          (Temp name) -> name : toVariableNames os
toVariableNames [] = []

-- get the set of variables used by a quadruple
use :: Quadruple -> Set.Set Name
use (Binary _ x y z) = Set.fromList $ toVariableNames [y, z]
use (Unary _ x y) = Set.fromList $ toVariableNames [y]
use (Assign x y) = Set.fromList $ toVariableNames [y]
use (Lbl _) = Set.empty
use (Goto _) = Set.empty
use (If _ x) = Set.fromList $ toVariableNames [x]
use (Par x) = Set.fromList $ toVariableNames [x]
use (Call _ x) = Set.fromList $ toVariableNames [x]
use (Fcall x _ y) = Set.fromList $ toVariableNames [y]
use Ret = Set.empty
use (Fret x) = Set.fromList $ toVariableNames [x]
use (Ldar x y z) = Set.fromList $ toVariableNames [y, z]
use (Star x y z) = Set.fromList $ toVariableNames [y, z]

-- get the set of variables defined by a quadruple
def :: Quadruple -> Set.Set Name
def (Binary _ x y z) = Set.fromList $ toVariableNames [x]
def (Unary _ x y) = Set.fromList $ toVariableNames [x]
def (Assign x y) = Set.fromList $ toVariableNames [x]
def (Lbl _) = Set.empty
def (Goto _) = Set.empty
def (If _ x) = Set.empty
def (Par x) = Set.empty
def (Call _ x) = Set.empty
def (Fcall x _ y) = Set.fromList $ toVariableNames [x]
def Ret = Set.empty
def (Fret x) = Set.empty
def (Ldar x y z) = Set.fromList $ toVariableNames [x]
def (Star x y z) = Set.fromList $ toVariableNames [x]

-- get the set of variables live before each statement
live :: Program -> [(Quadruple, Set.Set Name)]
live [q] = [(q, use q)]
live (q:qs) = (q, Set.union (use q) (out Set.\\ def q)) : (successor,out) : ls
              where ((successor,out):ls) = live qs
