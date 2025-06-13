import Inductive.Validate

abbrev NonemptyString := {s : String // s ≠ ""}

structure RawInput where
  name : String
  birthYear : String
deriving Repr

structure CheckedInput (thisYear : Nat) : Type where
  name : NonemptyString
  birthYear : {y : Nat // y > 1900 ∧ y ≤ thisYear}
deriving Repr

abbrev Field := String

inductive TreeError where
  | field : Field → String → TreeError
  | path : String → TreeError → TreeError
  | both : TreeError → TreeError → TreeError
deriving Repr

instance : Append TreeError where
  append := .both

def reportError (f : Field) (msg : String) : Validate TreeError α :=
  .errors (.field f msg)

def checkName (name : String) : Validate TreeError NonemptyString :=
  if h : name = "" then
    reportError "name" "Required"
  else pure ⟨name, h⟩

def checkYearIsNat (year : String) : Validate TreeError Nat :=
  match year.trim.toNat? with
  | none => reportError "birth year" "Must be digits"
  | some n => pure n

def checkBirthYear (thisYear year : Nat) : Validate TreeError {y : Nat // y > 1900 ∧ y ≤ thisYear} :=
  if h : year > 1900 then
    if h' : year ≤ thisYear then
      pure ⟨year, by simp [*]⟩
    else reportError "birth year" s!"Must be no later than {thisYear}"
  else reportError "birth year" "Must be after 1900"

def checkInput (year : Nat) (input : RawInput) : Validate TreeError (CheckedInput year) :=
  CheckedInput.mk <$>
    checkName input.name <*>
    (checkYearIsNat input.birthYear).andThen (checkBirthYear year ·)

inductive LegacyCheckedInput where
  | humanBefore1970 :
    (birthYear : {y : Nat // y > 999 ∧ y < 1970}) →
    String →
    LegacyCheckedInput
  | humanAfter1970 :
    (birthYear : {y : Nat // y > 1970}) →
    NonemptyString →
    LegacyCheckedInput
  | company :
    NonemptyString →
    LegacyCheckedInput
deriving Repr

def checkThat (condition : Bool) (field : Field) (msg : String) : Validate TreeError Unit :=
  if condition then pure () else reportError field msg

def checkCompany (input : RawInput) : Validate TreeError LegacyCheckedInput :=
  checkThat (input.birthYear == "FIRM") "birth year" "FIRM if a company" *>
    .company <$> checkName input.name

def checkSubtype {α : Type} [Append ε] (v : α) (p : α → Prop) [Decidable (p v)] (err : ε) : Validate ε {x : α // p x} :=
  if h : p v then pure ⟨v, h⟩ else .errors err

def checkHumanBefore1970 (input : RawInput) : Validate TreeError LegacyCheckedInput :=
  (checkYearIsNat input.birthYear).andThen fun y =>
    .humanBefore1970 <$>
      checkSubtype y (fun x => x > 999 ∧ x < 1970) (.field "birth year" "less than 1970") <*>
      pure input.name

def checkHumanAfter1970 (input : RawInput) : Validate TreeError LegacyCheckedInput :=
  (checkYearIsNat input.birthYear).andThen fun y =>
    .humanAfter1970 <$>
      checkSubtype y (· > 1970) (.field "birth year" "greater than 1970") <*>
      checkName input.name

def checkLegacyInput (input : RawInput) : Validate TreeError LegacyCheckedInput :=
  (checkCompany input).mapErrors (TreeError.path "Company" ·) <|>
  (checkHumanBefore1970 input).mapErrors (TreeError.path "Human before 1970" ·) <|>
  (checkHumanAfter1970 input).mapErrors (TreeError.path "Human after 1970" ·)

def indented (indent: String) : String → String
  | "" => ""
  | s => s.split (· == '\n')
    |>.map (s!"{indent}{·}")
    |>.intersperse "\n"
    |>.foldl (· ++ ·) ""

def report : TreeError → String
  | .field f msg => s!"{f}: {msg}"
  | .path p e => s!"For {p}:\n{indented "  " (report e)}"
  | .both e1 e2 => s!"{report e1}\n{report e2}"

def main : IO Unit := do
  let stdin ← IO.getStdin
  println! "Please input your name: "
  let name ← stdin.getLine
  println! "Please input your birth year (FIRM if a company): "
  let birthYear ← stdin.getLine
  let input := {name, birthYear}
  match checkLegacyInput input with
  |.ok res => IO.println s!"Success: {repr res}"
  |.errors errs => IO.println s!"Errors:\n{report errs}"
