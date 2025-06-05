class Monoid (α : Type) (bop : α → α → α) where
  id : α
  bop_assoc : ∀ a b c : α, bop (bop a b) c = bop a (bop b c)
  bop_id : ∀ a : α, bop a id = a
  id_bop : ∀ a : α, bop id a = a

class Abelian (α : Type) (bop : α → α → α) extends Monoid α bop where
  bop_comm : ∀ a b : α, bop a b = bop b a

instance : Abelian Nat Nat.add where
  id := 0
  bop_assoc := Nat.add_assoc
  bop_id := Nat.add_zero
  id_bop := Nat.zero_add
  bop_comm := Nat.add_comm

instance : Abelian Nat Nat.mul where
  id := 1
  bop_assoc := Nat.mul_assoc
  bop_id := Nat.mul_one
  id_bop := Nat.one_mul
  bop_comm := Nat.mul_comm

instance : Monoid String String.append where
  id := ""
  bop_assoc := String.append_assoc
  bop_id := String.append_empty
  id_bop := String.empty_append

instance : Monoid (List α) List.append where
  id := []
  bop_assoc := List.append_assoc
  bop_id := List.append_nil
  id_bop := List.nil_append

def fold (bop) [M : Monoid α bop] (xs : List α) : α :=
  xs.foldl bop M.id

#eval fold Nat.add [1, 2, 3, 4]
#eval fold Nat.mul [1, 2, 3, 4]
#eval fold String.append ["Hello, ", "world", "!"]
#eval fold List.append [[1, 2], [3, 4], [5, 6]]
