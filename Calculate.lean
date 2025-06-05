import Inductive.Many
import Structure.WithLog

abbrev Primitive (m : Type → Type) [Monad m] :=   Int →   Int → m Int
abbrev Monadic   (m : Type → Type) [Monad m] := m Int → m Int → m Int

inductive Expr (m : Type → Type) [Monad m] where
  | const : Int                           → Expr m
  | prim  : Primitive m → Expr m → Expr m → Expr m
  | mon   : Monadic   m → Expr m → Expr m → Expr m

def evaluateM [Monad m] : Expr m → m Int
  | Expr.const i         => pure i
  | Expr.prim op x y => do op (← evaluateM x) (← evaluateM y)
  | Expr.mon  op x y =>    op (  evaluateM x) (  evaluateM y)

def plus  [Monad m] : Primitive m := (return · + ·)
def minus [Monad m] : Primitive m := (return · - ·)
def times [Monad m] : Primitive m := (return · * ·)

def divideM [Monad m] (divzero : Int → m Empty) (x y : Int) : m Int :=
  if y == 0 then divzero x >>= Empty.elim else pure (x / y)
class Divide (m : outParam Type → Type) [Monad m] where
  divide : Int → Int → m Int
instance : Divide Option where divide := divideM fun _ => none
instance : Divide Many   where divide := divideM fun _ => Many.none
instance : Divide (Except String) where
  divide := divideM (Except.error s!"Tried to divide {·} by zero")

def choose : Monadic Many := Many.union

open Expr Divide in
#eval (evaluateM (mon choose
  (prim times
    (const 3)
    (mon choose (const 2) (const 5)))
  (mon choose
    (const 4)
    (prim (divide) (const 5) (const 0)))) : Many Int).takeAll
