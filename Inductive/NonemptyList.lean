abbrev NonemptyList (α : Type) := {l : List α // l.length > 0}

instance : Append (NonemptyList α) where
  append xs ys :=
    have h : (xs.val ++ ys.val).length > 0 := by
      rewrite [List.length_append]
      exact Nat.add_lt_add xs.property ys.property
    ⟨xs.val ++ ys.val, h⟩
