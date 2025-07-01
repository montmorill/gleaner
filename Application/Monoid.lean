class Monoid (α : Type) (mul : α → α → α) where
  id : α
  mul_assoc : ∀ a b c : α, mul (mul a b) c = mul a (mul b c)
  mul_id : ∀ a : α, mul a id = a
  id_mul : ∀ a : α, mul id a = a

class Abelian (α : Type) (mul : α → α → α) extends Monoid α mul where
  mul_comm : ∀ a b : α, mul a b = mul b a

instance : Abelian Nat Nat.add where
  id := 0
  mul_assoc := Nat.add_assoc
  mul_id := Nat.add_zero
  id_mul := Nat.zero_add
  mul_comm := Nat.add_comm

instance : Abelian Nat Nat.mul where
  id := 1
  mul_assoc := Nat.mul_assoc
  mul_id := Nat.mul_one
  id_mul := Nat.one_mul
  mul_comm := Nat.mul_comm

instance : Monoid String String.append where
  id := ""
  mul_assoc := String.append_assoc
  mul_id := String.append_empty
  id_mul := String.empty_append

instance : Monoid (List α) List.append where
  id := []
  mul_assoc := List.append_assoc
  mul_id := List.append_nil
  id_mul := List.nil_append

def fold (mul : α → α → α) [M : Monoid α mul] (xs : List α) : α :=
  xs.foldl mul M.id

#eval fold Nat.add [1, 2, 3, 4]
#eval fold Nat.mul [1, 2, 3, 4]
#eval fold String.append ["Hello, ", "world", "!"]
#eval fold List.append [[1, 2], [3, 4], [5, 6]]
