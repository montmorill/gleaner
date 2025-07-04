structure Config where
  indent : Nat := 2
  useASCII : Bool := false
  showAll : Bool := false
  showFullPath : Bool := true
  rootPaths : List String := []
  currentPrefix : String := ""

def configFromArgs : List String → Option Config
  | [] => some {} -- every fields default
  | "--ascii" :: xs => ({· with useASCII := true}) <$> configFromArgs xs
  | "--all" :: xs | "-a" :: xs => ({· with showAll := true}) <$> configFromArgs xs
  | path :: xs => do
    let config ← configFromArgs xs
    pure {config with rootPaths := path :: config.rootPaths}

def Config.preFile (cfg : Config) :=
  if cfg.useASCII then
    "|" ++ "".pushn '-' cfg.indent
  else
    "├" ++ "".pushn '─' cfg.indent

def Config.preDir (cfg : Config) :=
  (if cfg.useASCII then "|" else "│").pushn ' ' cfg.indent

def Config.fileName (cfg : Config) (file : String) : String :=
  s!"{cfg.currentPrefix}{cfg.preFile} {file}"

def Config.dirName (cfg : Config) (dir : String) : String :=
  s!"{cfg.currentPrefix}{cfg.preFile} {dir}/"

def Config.inDirectory (cfg : Config) : Config :=
  let newPrefix := s!"{cfg.preDir} {cfg.currentPrefix}"
  {cfg with showFullPath := false, currentPrefix := newPrefix}

inductive Entry where
  | file : String → Entry
  | dir : String → Entry
deriving Repr

def toEntry (path : System.FilePath) : IO (Option Entry) := do
  let components := path.components.reverse
  match components[0]? with
  | none => pure (some (.dir ""))
  | some "." | some ".." => pure none
  | some name =>
    pure (some (if (← path.isDir) then .dir name else .file name))

abbrev ConfigIO (α : Type) : Type := ReaderT Config IO α

instance [Monad m] : MonadReader ρ (ReaderT ρ m) where
  read := fun env => pure env

def showFileName (file : String) : ConfigIO Unit := do
  IO.println ((← read).fileName file)

def showDirName (dir : String) : ConfigIO Unit := do
  IO.println ((← read).dirName dir)

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
        if (← read).showFullPath then
          IO.println s!" {path}/"
        else
          showDirName name
        withReader (·.inDirectory)
          (doList (← path.readDir).toList
            (dirTree ·.path))

def usage : String :="\
Usage: doug [OPTION]... [FILE]...

Options:
  --ascii     Use ASCII characters to display the directory structure
  -a, --all   Show all entries including those starting with ."

def main (args : List String) : IO UInt32 := do
  match configFromArgs args with
  | some config =>
    if config.rootPaths.isEmpty then
      (dirTree (← IO.currentDir)).run config
    else
      doList config.rootPaths fun path => do
        (dirTree (← IO.FS.realPath path)).run config
    pure 0
  | none =>
    IO.eprintln s!"Didn't understand argument(s) {" ".intercalate args}\n"
    IO.eprintln usage
    pure 1
