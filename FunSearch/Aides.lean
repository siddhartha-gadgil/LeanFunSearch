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
  let fullContent := Json.mkObj [("git_hash", (← gitHash))
                                , ("content", content)]
  let fname : FilePath := "logs/" / (logFile ++ ".jsonl")
  appendFile fname fullContent.compress

def leanBlock (s: String) : String :=
  let tail :=
    s.splitOn "```lean" |>.getD 1 <|
      s.splitOn "```" |>.getD 1 s
  tail.splitOn "```" |>.get! 0

-- code of Adam Topaz
def parseFloat (s : String) : Except String Float :=
  match Lean.Json.parse s with
    | .ok (.num t) => .ok t.toFloat
    | _ => throw "Failed to parse as float."

def enclosedBlocks (start stop: String) (s: String) : List String :=
  s.splitOn start |>.tail |>.map (fun s => s.splitOn stop |>.get! 0)

#check List.splitOnP
#check IO.FS.lines

def enclosedLineBlocks (start stop: String) (lines : List String):
  List (List String) :=
  lines.splitOnP (fun s => (s.splitOn start).length > 1) |>.tail
  |>.map (fun ss => ss.takeWhile (fun s => (s.splitOn stop).length ≤  1))

#eval enclosedLineBlocks "```lean" "```" ["import", "```lean", "a", "b", "```", "c", "```lean", "d", "```", "e"]

#eval enclosedLineBlocks "<details>" "</details>" ["import", "-- <details>", "a", "b", "-- </details>", "c", "-- <details>", "d", "-- </details>", "e"]

/-- Extracts blocks to be used as sample code from a file. These should be enclosed in `<funsearch>` and `</funsearch>` tags. Generally these will
be in comments, e.g. `-- <funsearch>`. The lines containing the tags are not included in the output.
-/
def funBlocks (path: System.FilePath) : IO (List String) := do
  let lines ← IO.FS.lines path
  let blocks := enclosedLineBlocks "<funsearch>" "</funsearch>" lines.toList
  return blocks.map
    (fun ss => ss.foldl (fun s1 s2 => s1 ++ "\n" ++ s2) "")

def enclosedLines (start stop: String) (lines: List String) :
  List String :=
  (lines.splitOnP (fun s => (s.splitOn start).length > 1))[1]!.takeWhile (fun s => (s.splitOn stop).length ≤  1)

def funTailBlock (path: System.FilePath) : IO (String) := do
  let lines ← IO.FS.lines path
  let blocks := enclosedLines "<funtail>" "</funtail>" lines.toList
  return blocks.foldl (fun s1 s2 => s1 ++ "\n" ++ s2) ""

#eval enclosedLines "<funtail>" "</funtail>" ["import", "-- <funtail>", "a", "b", "-- </funtail>", "c", "-- <funtail>", "d", "-- </funtail>", "e"]
