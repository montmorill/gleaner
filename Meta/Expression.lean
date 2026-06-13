import Lean
import Meta.PrintExpr

open Lean

def nat : Expr := .const ``Nat []
def zero := Expr.const ``Nat.zero []
def succ (nat : Expr) : Expr := Expr.app (.const ``Nat.succ []) nat
def one := succ zero
def two := succ one

#check 1 + 2
#printExpr
  .app (.app (.const ``Nat.add []) one) two

#check 1 + 2
#printExpr
  mkAppN (.const ``Nat.add []) #[one , two]

#check fun x => 1 + x
#printExpr
  .lam `x nat (mkAppN (.const ``Nat.add []) #[one, .bvar 0]) .default

#check fun a b c => (b * a) + c
#printExpr
  .lam `a nat (.lam `b nat (.lam `c nat (
    mkAppN (.const ``Nat.add [])
      #[(mkAppN (.const ``Nat.mul []) #[.bvar 1, .bvar 2]), .bvar 0]
  ) .default) .default) .default

#check fun x y => x + y
#printExpr
  .lam `x nat (.lam `y nat (
    mkAppN (.const ``Nat.add []) #[.bvar 1, .bvar 0]
  ) .default) .default

#check fun x => String.append "hello, " x
#printExpr
  .lam `x (.const ``String []) (
    mkAppN (.const ``String.append []) #[.lit (.strVal "hello "), .bvar 0]
  ) .default

def prop : Expr := .sort Level.zero

#check ∀ x : Prop, x ∧ x
#printExpr
  .forallE `x prop (
    mkAppN (.const ``And []) #[.bvar 0, .bvar 0]
  ) .default

#check Nat → String

#check fun (p : Prop) => (λ hP : p => hP)

#check Type 6
