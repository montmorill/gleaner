inductive Many (α : Type) where
  | none : Many α
  | more : α → (Unit → Many α) → Many α

def Many.one (x : α) : Many α := Many.more x (fun () => Many.none)

def Many.union {α} : Many α → Many α → Many α
  | Many.none, ys => ys
  | Many.more x xs, ys => Many.more x (fun () => (xs ()).union ys)

instance : Append (Many α) where
  append := Many.union

def Many.fromList : List α → Many α
  | [] => Many.none
  | x :: xs => Many.more x (fun () => fromList xs)

def Many.take : Nat → Many α → List α
  | 0, _ => []
  | _ + 1, Many.none => []
  | n + 1, Many.more x xs => x :: (xs ()).take n

def Many.takeAll : Many α → List α
  | Many.none => []
  | Many.more x xs => x :: (xs ()).takeAll

def Many.bind (m : Many α) (f : α → Many β) : Many β :=
  match m with
  | Many.none => Many.none
  | Many.more x xs => f x ++ (xs ()).bind f

instance : Monad Many where
  pure := Many.one
  bind := Many.bind

instance : Coe (Option α) (Many α) where
  coe := (match · with
    | none => Many.none
    | some x => Many.one x)

def addsTo (goal : Nat) : List Nat → Many (List Nat)
  | [] => if goal = 0 then pure [] else Many.none
  | x :: xs => (if x ≤ goal
    then (x::·) <$> (addsTo (goal - x) xs)
    else Many.none) ++ addsTo goal xs

#eval (addsTo 10 [1, 2, 3, 4, 5]).takeAll

def Many.orElse : Many α → (Unit → Many α) → Many α
  | .none, ys => ys ()
  | .more x xs, ys => .more x (fun () => orElse (xs ()) ys)

instance : Alternative Many where
  failure := .none
  orElse := Many.orElse

def Many.countdown : Nat → Many Nat
  | 0 => .none
  | n + 1 => .more n (fun () => countdown n)

def evenDivisors (n : Nat) : Many Nat := do
  let k ← Many.countdown (n + 1)
  guard (k % 2 = 0)
  guard (n % k = 0)
  pure k
