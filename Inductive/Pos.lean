inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos

def Pos.add : Pos → Pos → Pos
  | Pos.one, k => Pos.succ k
  | Pos.succ n, k => Pos.succ (Pos.add n k)

instance : Add Pos where
  add := Pos.add

def Pos.mul : Pos → Pos → Pos
  | Pos.one, k => k
  | Pos.succ n, k => k + (Pos.mul n k)

instance : Mul Pos where
  mul := Pos.mul

instance : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

def Pos.toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ n => Pos.toNat n + 1

instance : ToString Pos where
  toString p := toString p.toNat

def Pos.comp : Pos → Pos → Ordering
  | Pos.one, Pos.one => Ordering.eq
  | Pos.one, Pos.succ _ => Ordering.lt
  | Pos.succ _, Pos.one => Ordering.gt
  | Pos.succ n, Pos.succ k => comp n k

instance : Ord Pos where
  compare := Pos.comp

instance : Hashable Pos where
  hash p := hash p.toNat

instance : Coe Pos Nat where
  coe:= Pos.toNat
