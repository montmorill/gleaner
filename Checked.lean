universe u v

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
