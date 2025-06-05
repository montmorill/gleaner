structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α
deriving Inhabited

def NonEmptyList.toList  : NonEmptyList α → List α
  | ⟨head, tail⟩ => head :: tail

def NonEmptyList.fromList (xs : List α) (valid : xs.length > 0) : NonEmptyList α :=
  match xs with
  | x :: xs => ⟨x, xs⟩

def NonEmptyList.get? : NonEmptyList α → Nat → Option α
  | xs, 0 => some xs.head
  | xs, n + 1 => xs.tail[n]?

abbrev NonEmptyList.inBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

def NonEmptyList.get (xs : NonEmptyList α) (i : Nat) (valid : xs.inBounds i) : α :=
  match i with
  | 0 => xs.head
  | n + 1 => xs.tail[n]

instance : GetElem (NonEmptyList α) Nat α NonEmptyList.inBounds where
  getElem := NonEmptyList.get

instance [Hashable α] : Hashable (NonEmptyList α) where
  hash xs := mixHash (hash xs.head) (hash xs.tail)

instance : HAppend (NonEmptyList α) (List α) (NonEmptyList α) where
  hAppend xs ys := ⟨xs.head, xs.tail ++ ys⟩

instance : HAppend (List α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys := ys ++ xs

instance : Append (NonEmptyList α) where
  append xs ys := ⟨xs.head, xs.tail ++ ys.toList⟩

instance [ToString α] : ToString (NonEmptyList α) where
  toString xs := "!" ++ toString xs.toList

instance : Coe (NonEmptyList α) (List α) where
  coe xs := xs.toList

def idahoSpiders := NonEmptyList.fromList [
    "Banded Garden Spider",
    "Long-legged Sac Spider",
    "Wolf Spider",
    "Hobo Spider",
    "Cat-faced Spider"
  ] (by decide)

example : NonEmptyList String :=
  { head := "Sparrow",
    tail := ["Duck", "Swan", "Magpie", "Eurasian coot", "Crow"]
  }

example (n : Nat) (k : Nat) : Bool :=
  n + k == k + n
