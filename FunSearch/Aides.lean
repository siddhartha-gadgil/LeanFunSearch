import Lean
import Lean.Meta
import Lean.Elab
import Lean.Parser
import Lean.Parser.Extension
import Std.Data.List.Basic

open Lean Meta Elab Parser Tactic


namespace funsearch

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

def gitHash : IO String := do
  let hash ← IO.Process.output { cmd := "git", args := #["rev-parse", "--short", "HEAD"] }
  return hash.stdout.trim

def appendLog (logFile: String) (content : Json) : IO Unit := do
  let dir : FilePath := "logs"
  if !(← dir.pathExists) then
    IO.FS.createDirAll dir
  let fullContent := Json.mkObj [("git_hast", (← gitHash))
                                , ("content", content)]
  let fname : FilePath := "logs/" / (logFile ++ ".jsonl")
  appendFile fname fullContent.compress

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
