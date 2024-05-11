import Lean
import Cache.IO
import FunSearch.Aides

open Lean Meta

namespace funsearch

structure ChatParams where
  n : Nat := 1
  temp : JsonNumber := 0.8
  stopTokens : Array String :=  #[]
  max_tokens : Nat := 1600


inductive ChatServer where
  | openAI (model: String := "gpt-3.5-turbo")
  | azure (deployment: String := "leanaide-gpt4")
      (model: String := "GPT-4")
  | generic (model: String) (url: String) (hasSystemMessage : Bool)

namespace ChatServer

def url : ChatServer → IO String
  | openAI _ =>
      return "https://api.openai.com/v1/chat/completions"
  | azure deployment _ =>
      azureURL deployment
  | generic _ url _ =>
      return url++"/v1/chat/completions"

def model : ChatServer → String
  | openAI model => model
  | azure _ model => model
  | generic model _ _ => model

def hasSystemMessage : ChatServer → Bool
  | openAI _ => true
  | azure _ _ => true
  | generic _ _ b => b

def authHeader? : ChatServer → IO (Option String)
  | openAI _ => do
    let key? ← openAIKey
    let key :=
    match key? with
      | some k => k
      | none => panic! "OPENAI_API_KEY not set"
    return some <|"Authorization: Bearer " ++ key
  | azure .. => do
    let key? ← azureKey
    let key :=
    match key? with
      | some k => k
      | none => panic! "AZURE_OPENAI_KEY not set"
    return some <| "api-key: " ++ key
  | generic .. =>
    return none

def mistralLocal : ChatServer :=
  .generic (model:="mistralai/Mistral-7B-Instruct-v0.2") "http://localhost:8000/v1" false

def query (server: ChatServer)(messages : Json)(params : ChatParams) : CoreM Json := do
  let dataJs := Json.mkObj [("model", server.model), ("messages", messages)
  , ("temperature", Json.num params.temp), ("n", params.n), ("max_tokens", params.max_tokens),
  ("stop", Json.arr <| params.stopTokens |>.map Json.str)
  ]
  let data := dataJs.pretty
  trace[Translate.info] "Model query: {data}"
  let url ← server.url
  let authHeader? ← server.authHeader?
  -- IO.eprintln s!"Querying {url} at {← IO.monoMsNow }"
  -- let start ← IO.monoMsNow
  let baseArgs :=
    #[url, "-X", "POST", "-H", "Content-Type: application/json"]
  let args := match authHeader? with
    | some h => #["-H", h] ++ baseArgs
    | none => baseArgs
  let output ← Cache.IO.runCurl (args ++ #["--data", data])
  trace[Translate.info] "Model response: {output}"
  let queryJs := Json.mkObj [
    ("url", Json.str url),
    ("arguments", Json.arr <| baseArgs.map (Json.str)),
    ("data", data)]
  -- IO.eprintln s!"Received response from {url} at {← IO.monoMsNow }; time taken: {(← IO.monoMsNow) - start}"
  match Lean.Json.parse output with
  | Except.ok j =>
    appendLog "chat_queries"
      (Json.mkObj [("query", queryJs), ("success", true), ("response", j)])
    return j
  | Except.error e =>
    appendLog "chat_queries"
      (Json.mkObj [("query", queryJs), ("success", false), ("error", e), ("response", output)])
    panic! s!"Error parsing JSON: {e}; source: {output}"

def stringsFromJson (json: Json) : CoreM (Array String) := do
  match json.getObjVal? "choices" with
  | Except.ok js =>
  let outArr : Array String ←
    match js.getArr? with
    | Except.ok arr =>
        let parsedArr : Array String ←
          arr.filterMapM <| fun js =>
            match js.getObjVal? "message" with
            | Except.ok jsobj =>
                match jsobj.getObjVal? "content" with
                | Except.ok jsstr =>
                  match jsstr.getStr? with
                  | Except.ok str => pure (some str)
                  | Except.error e =>
                    throwError m!"json string expected but got {js}, error: {e}"
                | Except.error _ =>
                  throwError m!"no content field in {jsobj}"
            | Except.error _ =>
                throwError m!"no message field in {js}"
        pure parsedArr
    | Except.error e =>
      throwError m!"json parsing error: {e} when getting array from {js}"
  | Except.error e =>
    throwError m!"json parsing error: {e} when getting choices field from {json}"


def queryTextsForMessages (server: ChatServer)(messages : Json)
    (params : ChatParams) : CoreM <| Array String := do
  let response ← server.query messages params
  stringsFromJson response


def sysPrompt: String := "You are a Lean prover and Mathematics assistant. Give Lean code or Mathematical answers. Be precise and concise and give ONLY the code or mathematics asked for. If you are giving Lean code, either give ONLY the code or enclose in fenced code blocks between ```lean and ```"

def sysResponse: String := "Sure. I will give precise and concise responses following the instructions."

def sysMessage (server: ChatServer) : Array Json :=
  if server.hasSystemMessage then
     #[Json.mkObj [("role", "system"), ("content", sysPrompt)]]
  else
    #[Json.mkObj [("role", "user"), ("content", sysPrompt)],
      Json.mkObj [("role", "assistant"), ("content", sysResponse)]]

def simpleMessages (server: ChatServer) (instructions: String)  : CoreM Json := do
  let main := Json.mkObj [("role", "user"), ("content", instructions)]
  if server.hasSystemMessage then
    let system := Json.mkObj [("role", "system"), ("content", sysPrompt)]
    return Json.arr #[system, main]
  else
    return Json.arr #[main]

def queryTexts (server: ChatServer) (instructions: String)
    (params : ChatParams) : CoreM (Array String) := do
  let messages ← server.simpleMessages instructions
  queryTextsForMessages server messages params

end ChatServer

structure ChatExample where
  user : String
  assistant : String

def ChatExample.messages (ex : ChatExample) : List Json :=
  [Json.mkObj [("role", "user"), ("content", ex.user)],
    Json.mkObj [("role", "assistant"), ("content", ex.assistant)]]

abbrev ToChatExample := String × Json → Option ChatExample
