import Lean
import Meta.PrintExpr

open Lean

def nat : Expr := .const ``Nat []
def zero := Expr.const ``Nat.zero []
def succ (nat : Expr) : Expr := Expr.app (.const ``Nat.succ []) nat
def one := succ zero
def two := succ one

#printExpr
  .app (.app (.const ``Nat.add []) one) two

#printExpr
  mkAppN (.const ``Nat.add []) #[one , two]

#printExpr
  .lam `x nat (mkAppN (.const ``Nat.add []) #[one, .bvar 0]) .default

#printExpr
  .lam `a nat (.lam `b nat (.lam `c nat
    (mkAppN (.const ``Nat.add [])
      #[(mkAppN (.const ``Nat.mul []) #[.bvar 1, .bvar 2]), .bvar 0])
  .default) .default) .default
