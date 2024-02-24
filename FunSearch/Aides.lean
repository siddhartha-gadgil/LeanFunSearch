import Lean
import Lean.Meta
import Lean.Elab
import Lean.Parser
import Lean.Parser.Extension
import Std.Data.List.Basic

open Lean Meta Elab Parser Tactic

def Lean.Expr.view (expr: Expr) : MetaM String := do
  let expr ← instantiateMVars expr
  let fmt ← PrettyPrinter.ppExpr expr
  return fmt.pretty

def getStr! (j : Json) : String :=
  j.getStr?.toOption.get!


def EIO.runToIO' (eio: EIO Exception α) : IO α  := do
  match ←  eio.toIO' with
  | Except.ok x =>
      pure x
  | Except.error e =>
      let msg ← e.toMessageData.toString
      IO.throwServerError msg

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


def openAIKey : IO (Option String) := IO.getEnv "OPENAI_API_KEY"

def azureKey : IO (Option String) := IO.getEnv "AZURE_OPENAI_KEY"

def azureEndpoint : IO (Option String) := IO.getEnv "AZURE_OPENAI_ENDPOINT"

def azureURL (deployment: String := "leanaide-gpt4") : IO String := do
  let endpoint ← azureEndpoint
  match endpoint with
  | none => throw <| IO.userError "AZURE_OPENAI_ENDPOINT not set"
  | some endpoint =>
    return s!"{endpoint}/openai/deployments/{deployment}/chat/completions?api-version=2023-05-15"

def openAIURL : IO String := do
  pure "https://api.openai.com/v1/chat/completions"


open System IO.FS
def appendFile (fname : FilePath) (content : String) : IO Unit := do
  let h ← Handle.mk fname Mode.append
  h.putStrLn content
  h.flush

def appendLog (logFile: String) (content : Json) : IO Unit := do
  let dir : FilePath := "rawdata"
  if !(← dir.pathExists) then
    IO.FS.createDirAll dir
  let fname : FilePath := "rawdata/" / ("log_" ++ logFile ++ ".jsonl")
  appendFile fname content.compress

def gitHash : IO String := do
  let hash ← IO.Process.output { cmd := "git", args := #["rev-parse", "--short", "HEAD"] }
  return hash.stdout.trim

def colEqSegments (s: String) : List String :=
  let pieces := s.splitOn ":="
  match pieces with
  | [] => []
  | head :: tail =>
    tail.scanl (fun acc x => acc ++ ":=" ++ x) head |>.map (String.trim)

def splitColEqSegments (ss: Array String) : Array String :=
  ss.toList.bind colEqSegments |>.toArray

def leanBlock (s: String) : String :=
  let fullSplit := s.splitOn "```lean"
  let split := if fullSplit.length > 1
    then fullSplit.get! 1 else
    s.splitOn "```" |>.get! 1
  split.splitOn "```" |>.get! 0

-- code of Adam Topaz
def parseFloat (s : String) : Except String Float :=
  match Lean.Json.parse s with
    | .ok (.num t) => .ok t.toFloat
    | _ => throw "Failed to parse as float."
