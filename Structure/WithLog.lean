universe u v

structure WithLog (logged : Type) (α : Type u) where
  val : α
  logs : List logged
deriving Repr

instance : Monad (WithLog logged) where
  pure a := ⟨a, []⟩
  bind action next :=
    let result := next action.val
    ⟨result.val, result.logs ++ action.logs⟩

structure WithLogT (logged : Type) (m : Type u → Type v) (α : Type u) : Type v where
  mk ::
  run : m (WithLog logged α)

instance [Monad m] : Monad (WithLogT logged m) where
  pure x := WithLogT.mk (pure (pure x))
  bind action next := WithLogT.mk do
    let ⟨val, log⟩ ← action.run
    let result ← (next val).run
    pure ⟨result.val, result.logs ++ log⟩

instance [Monad m] : MonadLift m (WithLogT logged m) where
  monadLift action := WithLogT.mk do
    pure (pure (← action))
