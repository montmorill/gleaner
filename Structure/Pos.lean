structure Pos where
  succ ::
  pred : Nat

def Pos.add (a b : Pos): Pos :=
  Pos.succ (a.pred + b.pred + 1)

instance : Add Pos where
  add := Pos.add

def Pos.mul (a b : Pos): Pos :=
  Pos.succ (a.pred * b.pred + a.pred + b.pred)

instance : Mul Pos where
  mul := Pos.mul

def Pos.toNat : Pos → Nat
  | ⟨pred⟩ => pred + 1

instance : OfNat Pos (n + 1) where
  ofNat := Pos.succ n

instance : ToString Pos where
  toString p := toString p.toNat

def addNatPos : Nat → Pos → Pos
  | 0, p => p
  | n + 1, p => addNatPos n (p + 1)

instance : HAdd Nat Pos Pos where
  hAdd := addNatPos

instance : HAdd Pos Nat Pos where
  hAdd := fun p n => n + p

instance : Ord Pos where
  compare := fun ⟨a⟩ ⟨b⟩ => compare a b

instance : Hashable Pos where
  hash p := hash p.toNat

instance : Coe Pos Nat where
  coe:= Pos.toNat
