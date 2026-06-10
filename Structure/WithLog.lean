universe u v

structure WithLog (logged : Type) (α : Type u) where
  val : α
  log : List logged
deriving Repr

instance : Monad (WithLog logged) where
  pure a := ⟨a, []⟩
  bind action next :=
    let result := next action.val
    ⟨result.val, result.log ++ action.log⟩

structure WithLogT (logged : Type) (m : Type u → Type v) (α : Type u) : Type v where
  mk ::
  run : m (WithLog logged α)

instance [Monad m] : Monad (WithLogT logged m) where
  pure x := WithLogT.mk (pure (pure x))
  bind action next := WithLogT.mk do
    let ⟨val, log⟩ ← action.run
    let ⟨val', log'⟩ ← (next val).run
    pure ⟨val', log' ++ log⟩

instance [Monad m] : MonadLift m (WithLogT logged m) where
  monadLift action := WithLogT.mk do
    pure (pure (← action))

abbrev WithLogT' (logged : Type) (m : Type u → Type v) (α : Type u) : Type v :=
  WithLog logged (m α)

instance [Monad m] : Monad (WithLogT' logged m) where
  pure x := ⟨pure x, []⟩
  bind action next :=
    ⟨do let a ← action.val
        (next a).val,
     action.log⟩

instance [Monad m] : MonadLift m (WithLogT' logged m) where
  monadLift action := ⟨action, []⟩

instance : Coe α (WithLog logged α) where
  coe action := ⟨action, []⟩

instance [Monad m] : Coe α (WithLogT' logged m α) where
  coe action := ⟨pure action, []⟩
