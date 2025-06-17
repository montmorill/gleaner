structure Config where
  useASCII : Bool := false
  showAll : Bool := false
  currentPrefix : String := ""

def configFromArgs : List String → Option Config
  | [] => some {} -- every fields default
  | "--ascii" :: xs => ({· with useASCII := true}) <$> configFromArgs xs
  | "-a" :: xs | "--all" :: xs => ({· with showAll := true}) <$> configFromArgs xs
  | _ => none

def Config.preFile (cfg : Config) :=
  if cfg.useASCII then "|--" else "├──"

def Config.preDir (cfg : Config) :=
  if cfg.useASCII then "|  " else "│  "

def Config.fileName (cfg : Config) (file : String) : String :=
  s!"{cfg.currentPrefix}{cfg.preFile} {file}"

def Config.dirName (cfg : Config) (dir : String) : String :=
  s!"{cfg.currentPrefix}{cfg.preFile} {dir}/"

def Config.inDirectory (cfg : Config) : Config :=
  {cfg with currentPrefix := cfg.preDir ++ " " ++ cfg.currentPrefix}

inductive Entry where
  | file : String → Entry
  | dir : String → Entry

def toEntry (path : System.FilePath) : IO (Option Entry) := do
  match path.components.getLast? with
  | none => pure (some (.dir ""))
  | some "." | some ".." => pure none
  | some name =>
    pure (some (if (← path.isDir) then .dir name else .file name))

abbrev ConfigIO (α : Type) : Type := ReaderT Config IO α

instance [Monad m] : MonadReader ρ (ReaderT ρ m) where
  read := fun env => pure env

def showFileName (file : String) : ConfigIO Unit := do
  IO.println s!"{(← read).currentPrefix} {file}"

def showDirName (dir : String) : ConfigIO Unit := do
  IO.println s!"{(← read).currentPrefix} {dir}/"

def shouldShow (name : String) : ConfigIO Bool := do
  pure ((¬ name.startsWith ".") ∨ (← read).showAll)

def doList [Applicative f] : List α → (α → f Unit) → f Unit
  | [], _ => pure ()
  | x :: xs, action =>
    action x *>
    doList xs action

partial def dirTree (path : System.FilePath) : ConfigIO Unit := do
  match ← toEntry path with
    | none => pure ()
    | some (.file name) =>
      if ← shouldShow name then showFileName name
    | some (.dir name) =>
      if ← shouldShow name then
        showDirName name
        let contents ← path.readDir
        withReader (·.inDirectory)
          (doList contents.toList fun d =>
            dirTree d.path)

def usage : String :="\
Usage: doug [OPTION]...

Options:
  --ascii     Use ASCII characters to display the directory structure
  -a, --all   Show all entries including those starting with ."

def main (args : List String) : IO UInt32 := do
  match configFromArgs args with
  | some config =>
    (dirTree (← IO.currentDir)).run config
    pure 0
  | none =>
    IO.eprintln s!"Didn't understand argument(s) {" ".intercalate args}\n"
    IO.eprintln usage
    pure 1
