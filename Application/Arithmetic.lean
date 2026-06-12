import Inductive.Many
import Structure.WithLog

structure Primitive (m : Type → Type) [Monad m] where
  label : String
  run : m Int → m Int → m Int

def plus  [Monad m] : Primitive m := ⟨"plus",  fun a b => do pure ((←a) + (←b))⟩
def minus [Monad m] : Primitive m := ⟨"minus", fun a b => do pure ((←a) - (←b))⟩
def times [Monad m] : Primitive m := ⟨"times", fun a b => do pure ((←a) * (←b))⟩

class Divide (m : Type → Type) extends Monad m where
  dividezero : Int → m Empty

instance : Divide Option where
  dividezero _ := .none

instance : Divide Many where
  dividezero _ := .none

instance : Divide (Except String) where
  dividezero dividend := .error s!"Tried to divide {dividend} by zero"

def divide [Divide m] : Primitive m :=
  let aux : Int → Int → m Int
    | x, 0 => Divide.dividezero x >>= Empty.elim
    | x, y => pure (x / y)
  ⟨"divide", fun a b => do aux (←a) (←b)⟩

-- def choose : Many Int → Many I→\->  := Many.union

def trace [Monad m] (prim : Primitive m) :
    Primitive (WithLogT' (String × m Int × m Int) m) :=
  ⟨prim.label, fun a b =>
    let res := prim.run a.val b.val
    ⟨res, a.log ++ b.log ++ [(prim.label, a.val, b.val)]⟩⟩

#eval (let trace := trace (m := Id);
    (times.run
      ((trace plus).run (pure 1) (pure 2))
      ((trace minus).run (pure 3) (pure 4))))
