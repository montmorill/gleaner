structure WithLog (logged : Type) (α : Type) where
  log : List logged
  val : α
deriving Repr

instance : Monad (WithLog logged) where
  pure x := ⟨[], x⟩
  bind m f :=
    let ⟨log, val⟩ := f m.val
    ⟨m.log ++ log, val⟩

abbrev WithLogM (m : Type → Type) [Monad m] (logged : Type): Type → Type :=
  fun α => WithLog logged (m α)

instance : Monad (WithLogM Option logged) where
  pure a := ⟨[], pure a⟩
  bind m f := match m.val with
    | none => ⟨m.log, none⟩
    | some x =>
      let ⟨log, val⟩ := f x
      ⟨m.log ++ log, val⟩

instance : Monad (WithLogM (Except ε) logged) where
  pure a := ⟨[], pure a⟩
  bind m f := match m.val with
    | .error e => ⟨m.log, Except.error e⟩
    | .ok x => let ⟨log, val⟩ := f x
      ⟨m.log ++ log, val⟩
