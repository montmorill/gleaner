import Lean

open Lean Elab Command Term Meta in
elab "#printExpr" term:term : command => unsafe liftTermElabM do
    let exprType ← mkConst ``Expr
    let expr ← elabTerm term (some exprType)
    let val ← evalExpr Expr exprType expr
    let stx ← PrettyPrinter.delab val
    let valType ← inferType val
    logInfo m!"{stx} : {← ppExpr valType}"
