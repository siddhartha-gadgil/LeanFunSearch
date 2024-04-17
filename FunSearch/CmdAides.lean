import Lean

/-!
Helpers for command line programs. Clashes with `LeanAide` so should
not be imported in the same file.
-/

open Lean

def EIO.runToIO' (eio: EIO Exception α) : IO α  := do
  match ←  eio.toIO' with
  | Except.ok x =>
      pure x
  | Except.error e =>
      let msg ← e.toMessageData.toString
      IO.throwServerError msg

def Meta.runToIO (x : MetaM α)(ctx : Core.Context)(env: Environment) :
    IO α := do
  let core := x.run' {}
  let io? := core.run' ctx {env := env}
  EIO.runToIO' io?


def EIO.spawnToIO (eio: EIO Exception α) : IO <| Task <| IO α  := do
  let task ←  eio.asTask (prio := Task.Priority.max)
  return task.map (fun eio =>
    match eio with
    | Except.ok x =>
        pure x
    | Except.error e => do
        let msg ←  e.toMessageData.toString
        IO.throwServerError msg)

def EIO.asyncIO (eio: EIO Exception α) : IO α  := do
  let task ← EIO.spawnToIO eio
  task.get

def threadNum : IO Nat := do
  try
    let info ←  IO.FS.readFile <| System.mkFilePath ["/", "proc", "cpuinfo"]
    return (info.splitOn "processor" |>.length) - 1
  catch _ =>
    return 4


@[inline] protected def Except.mapM [Monad m] (f : α → m β)
    (o : Except ε α) : m (Except ε β) := do
  match o with
  | Except.ok a => return Except.ok (← f a)
  | Except.error e => return Except.error e

-- #eval Lean.moduleNameOfFileName "FunSearch/Examples/SimpleNat.lean" (System.FilePath.mk "/home/gadgil/code/LeanFunSearch")

def modulePath (moduleName: Name) : IO (System.FilePath) := do
  let pieces := moduleName.components.map
    (fun n => (n.toString : System.FilePath))
  return pieces.foldl (· / ·) (← IO.currentDir)
