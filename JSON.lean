inductive JSON where
  | null    :                        JSON
  | boolean : Bool                 → JSON
  | number  : Float                → JSON
  | string  : String               → JSON
  | array   : List JSON            → JSON
  | object  : List (String × JSON) → JSON

instance : Coe Unit JSON          := ⟨fun _ => JSON.null   ⟩
instance : Coe Bool JSON                   := ⟨JSON.boolean⟩
instance : Coe Float JSON                  := ⟨JSON.number ⟩
instance : Coe String JSON                 := ⟨JSON.string ⟩
instance : Coe (List JSON) JSON            := ⟨JSON.array  ⟩
instance : Coe (List (String × JSON)) JSON := ⟨JSON.object ⟩

def dropDecimals (numString : String) : String :=
  if numString.contains '.' then
    let noTrailingZeros := numString.dropRightWhile (· == '0')
    noTrailingZeros.dropRightWhile (· == '.')
  else numString

def escape (s : String) :=
  s.data.map (fun c
  => match c with
    | '\\' => "\\\\"
    | '"' => "\\\""
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | _ => c.toString)
  |>.foldl (fun acc c => acc ++ c) ""

partial def JSON.toString: (val : JSON) → String
  | true => "true"
  | false => "false"
  | null => "null"
  | string s => "\"" ++ escape s ++ "\""
  | number n => dropDecimals n.toString
  | object members =>
    "{" ++ ", ".intercalate (members.map fun ((key, value)) =>
      "" ++ toString key ++ ": " ++ toString value) ++ "}"
  | array elements =>
    "[" ++ ", ".intercalate (elements.map toString) ++ "]"

instance : ToString JSON where
  toString := JSON.toString
