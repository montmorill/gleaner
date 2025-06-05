structure PPoint (α : Type) where
  x: α
  y: α

instance [HAdd α β γ] : HAdd (PPoint α)  (PPoint β) (PPoint γ) where
  hAdd p1 p2 := ⟨p1.x + p2.x, p1.y + p2.y⟩

instance [HMul α β γ] : HMul (PPoint α) β (PPoint γ) where
  hMul p s := ⟨p.x * s, p.y * s⟩

instance [HMul α β γ] : HMul α (PPoint β) (PPoint γ) where
  hMul s p := ⟨s * p.x, s * p.y⟩

instance : Functor PPoint where
  map f p := { x := f p.x, y := f p.y }
