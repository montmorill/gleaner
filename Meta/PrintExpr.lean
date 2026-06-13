import Lean

open Lean Elab Command Term Meta in
elab "#printExpr" term:term : command => unsafe liftTermElabM do
    let exprType ← mkConst ``Expr
    let expr ← elabTerm term (some exprType)
    let expr' ← instantiateMVars expr
    let val ← evalExpr Expr exprType expr'
    let stx ← PrettyPrinter.delab val
    let valType ← inferType val
    let fmt := m!"{stx} : {← ppExpr valType}"
    logInfo fmt
