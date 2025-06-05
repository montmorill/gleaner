structure WithLog (logged : Type) (α : Type) where
  log : List logged
  val : α
