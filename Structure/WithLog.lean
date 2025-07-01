structure WithLog (logged : Type) (α : Type) where
  log : List logged
  val : α
deriving Repr

def WithLogT (logged : Type) (m : Type → Type) (α : Type) :=
  m (WithLog logged α)

variable {logged : Type} {m : Type → Type} [Monad m]
