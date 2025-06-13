inductive Validate (ε : Type) [Append ε] (α : Type) : Type where
  | ok : α → Validate ε α
  | errors : ε → Validate ε α
deriving Repr

instance [Append ε] : Functor (Validate ε) where
  map f
   | .ok x => .ok (f x)
   | .errors errs => .errors errs

instance [Append ε] : Applicative (Validate ε) where
  pure := .ok
  seq f x :=
    match f with
    | .ok g => g <$> (x ())
    | .errors errs =>
      match x () with
      | .ok _ => .errors errs
      | .errors errs' => .errors (errs ++ errs')

def Validate.andThen [Append ε] (val : Validate ε α) (next : α → Validate ε β) : Validate ε β :=
  match val with
  | .errors errs => .errors errs
  | .ok x => next x

instance [Append ε] : OrElse (Validate ε α) where
  orElse a b :=
    match a with
    | .ok x => .ok x
    | .errors errs =>
      match b () with
      | .ok x => .ok x
      | .errors errs' => .errors (errs ++ errs')

def Validate.mapErrors [Append ε] [Append ε'] (val : Validate ε α) (f : ε → ε') : Validate ε' α :=
  match val with
  | .ok x => .ok x
  | .errors errs => .errors (f errs)
