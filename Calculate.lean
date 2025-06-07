import Inductive.Many

structure Primitive (m : Type → Type) [Monad m] where
  name : String
  call : m Int → m Int → m Int
inductive Expr (m : Type → Type) [Monad m] where
  | const : Int                           → Expr m
  | prim  : Primitive m → Expr m → Expr m → Expr m

def evaluateM [Monad m] : Expr m → m Int
  | Expr.const i         => pure i
  | Expr.prim op x y => do op.call (evaluateM x) (evaluateM y)

def plain [Monad m] (f : Int → Int → Int) : m Int → m Int → m Int :=
  fun x y => do pure (f (← x) (← y))

def plus  [Monad m] : Primitive m := ⟨"plus",  plain (· + ·)⟩
def minus [Monad m] : Primitive m := ⟨"minus", plain (· - ·)⟩
def times [Monad m] : Primitive m := ⟨"times", plain (· * ·)⟩

class Divide (m : Type → Type) extends Monad m where dividezero : Int → m Empty
instance : Divide Option where dividezero _ := .none
instance : Divide Many   where dividezero _ := .none
instance : Divide (Except String) := ⟨(Except.error s!"Tried to divide {·} by zero")⟩

def divide [Divide m] : Primitive m := open Divide in
  let inner := fun x y =>
    if y == 0 then dividezero x >>= Empty.elim
    else pure (x / y)
  ⟨"divide", fun x y => do inner (← x) (← y)⟩

def choose : Primitive Many := ⟨"choose", Many.union⟩

class Trace (m : Type → Type) extends Monad m where
  trace : String → Int → m Int

instance : Trace (Except String) where
  trace _ x := pure x

-- open Expr in
-- #eval evaluateM (
--   prim (trace times)
--     (prim (trace plus) (const 1) (const 2))
--     (prim (trace minus) (const 3) (const 4))
-- )
--{ log := [(Prim.plus, 1, 2), (Prim.minus, 3, 4), (Prim.times, 3, -1)], val := -3 }
