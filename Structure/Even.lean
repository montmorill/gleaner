structure Even where
  double ::
  half : Nat

def Even.add: Even → Even → Even
  | ⟨a⟩, ⟨b⟩ => ⟨a + b⟩

instance : Add Even where
  add := Even.add

def Even.mul: Even → Even → Even
  | ⟨a⟩, ⟨b⟩ => ⟨2 * a * b⟩

instance : Mul Even where
  mul := Even.mul

def Even.toNat: Even → Nat
  | ⟨half⟩ => 2 * half

instance : OfNat Even Nat.zero where
  ofNat := ⟨0⟩

instance [OfNat Even n] : OfNat Even (n + 2) where
  ofNat := ⟨(n + 2)/ 2⟩

-- instance [OfNat Even n] : OfNat Even (n + 256) where
--   ofNat := ⟨(n + 256)/ 2⟩

-- instance [OfNat Even n] : OfNat Even (n + 4096) where
--   ofNat := ⟨(n + 4096)/ 2⟩

instance : ToString Even where
  toString p := toString p.toNat
