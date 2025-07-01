universe u v

class AppendSemigroup (G : Type u) extends Append G where
  append_assoc : ∀ a b c : G, a ++ b ++ c = a ++ (b ++ c)

class CheckedFunctor (f : Type u → Type v) extends Functor f where
  map_identity : ∀ (α : Type u) (x : f α),
    id <$> x = x

  map_composition : ∀ (α β γ : Type u) (g : β → γ) (h : α → β) (x : f α),
    (g ∘ h) <$> x = g <$> (h <$> x)

class CheckedApplicative (f : Type u → Type v)
  extends CheckedFunctor f, Applicative f where

  ap_identity : ∀ (α : Type u) (x : f α),
    pure id <*> x = x

  ap_composition : ∀ (α β γ : Type u) (u : f (β → γ)) (v : f (α → β)) (w : f α),
    pure Function.comp <*> u <*> v <*> w = u <*> (v <*> w)

  homomorphism : ∀ (α β : Type u) (g : α → β) (x : α),
    pure g <*> pure x = pure (g x)

  interchange : ∀ (α β : Type u) (u : f (α → β)) (y : α),
    u <*> pure y = pure (. $ y) <*> u

class CheckedMonad (m : Type u → Type v)
  extends CheckedApplicative m, Monad m where

  bind_left_identity : ∀ (α β : Type u) (a : α) (f : α → m β),
    pure a >>= f = f a

  bind_right_identity : ∀ (α : Type u) (c : m α),
    c >>= pure = c

  bind_associativity : ∀ (α β γ : Type u) (c : m α) (f : α → m β) (g : β → m γ),
    (c >>= f) >>= g = c >>= (fun x => f x >>= g)

inductive Validate (ε : Type) (α : Type) where
  | ok : α → Validate ε α
  | errors : ε → Validate ε α
deriving Repr, Inhabited

namespace Validate

instance : CheckedFunctor (Validate ε) where
  map := fun f =>
    λ | .ok x => .ok (f x)
      | .errors errs => .errors errs

  map_identity := by
    intro α x
    cases x <;> rfl

  map_composition := by
    intro α β γ g h x
    cases x <;> rfl

instance [AppendSemigroup ε] : CheckedApplicative (Validate ε) where
  pure := .ok

  seq f x :=
    match f with
    | .ok g =>
      match x () with
      | .ok a => .ok (g a)
      | .errors e => .errors e
    | .errors e1 =>
      match x () with
      | .ok _ => .errors e1
      | .errors e2 => .errors (e1 ++ e2)

  ap_identity := by
    intro α x
    cases x with
    | ok _ => rfl
    | errors _ => rfl

  ap_composition := by
    intro α β γ u v w
    cases u with
    | ok g =>
      cases v with
      | ok h =>
        cases w with
        | ok a => rfl
        | errors e => rfl
      | errors e =>
        cases w with
        | ok _ => rfl
        | errors e' => rfl
    | errors e1 =>
      cases v with
      | ok _ =>
        cases w with
        | ok _ => rfl
        | errors e2 => rfl
      | errors e2 =>
        cases w with
        | ok _ => rfl
        | errors e3 => simp [AppendSemigroup.append_assoc]

  homomorphism := by
    intro α β g x
    rfl

  interchange := by
    intro α β u y
    cases u with
    | ok g => rfl
    | errors e => rfl
