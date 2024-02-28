import Lean
import Cache.IO
import FunSearch.Aides

open Lean Meta

structure ChatParams where
  n : Nat := 1
  temp : JsonNumber := 0.8
  stopTokens : Array String :=  #[]
  model : String := "gpt-3.5-turbo"
  max_tokens : Nat := 1600


inductive ChatServer where
  | openAI
  | azure (deployment: String := "leanaide-gpt4-32")
  | generic (url: String) (hasSystem : Bool := true)

namespace ChatServer

def url : ChatServer → IO String
  | openAI =>
      return "https://api.openai.com/v1/chat/completions"
  | azure deployment =>
      azureURL deployment
  | generic url _ =>
      return url++"/v1/chat/completions"

def hasSystem : ChatServer → Bool
  | openAI => true
  | azure _ => true
  | generic _ b => b

def authHeader? : ChatServer → IO (Option String)
  | openAI => do
    let key? ← openAIKey
    let key :=
    match key? with
      | some k => k
      | none => panic! "OPENAI_API_KEY not set"
    return some <|"Authorization: Bearer " ++ key
  | azure _ => do
    let key? ← azureKey
    let key :=
    match key? with
      | some k => k
      | none => panic! "AZURE_OPENAI_KEY not set"
    return some <| "api-key: " ++ key
  | generic _ _ =>
    return none

def query (server: ChatServer)(messages : Json)(params : ChatParams) : CoreM Json := do
  let dataJs := Json.mkObj [("model", params.model), ("messages", messages)
  , ("temperature", Json.num params.temp), ("n", params.n), ("max_tokens", params.max_tokens),
  ("stop", Json.arr <| params.stopTokens |>.map Json.str)
  ]
  let data := dataJs.pretty
  trace[Translate.info] "Model query: {data}"
  let url ← server.url
  let authHeader? ← server.authHeader?
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
  match Lean.Json.parse output with
  | Except.ok js =>
    appendLog "chat_queries"
      (Json.mkObj [("query", queryJs), ("success", true), ("response", js)])
    let outJson? :=
        (js.getObjVal? "choices")
    match outJson? with
    | Except.ok outJson =>
      pure outJson
    | Except.error e =>
      appendLog "chat_queries"
        (Json.mkObj [("query", queryJs), ("success", false), ("error", e), ("response", output)])
      throwError m!"No choices in JSON: {js.compress}"
  | Except.error e =>
    appendLog "chat_queries"
      (Json.mkObj [("query", queryJs), ("success", false), ("error", e), ("response", output)])
    throwError m!"Error parsing JSON: {e}; source: {output}"

def stringsFromJson (json: Json) : CoreM (Array String) := do
  let outArr : Array String ←
    match json.getArr? with
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
    | Except.error e => throwError m!"json parsing error: {e}"

def queryTexts (server: ChatServer)(messages : Json)
    (params : ChatParams) : CoreM <| Array String := do
  let response ← server.query messages params
  stringsFromJson response


def sysPrompt: String := "You are a Lean prover and Mathematics assistant. Give Lean code or Mathematical answers. Be precise and concise and give ONLY the code or mathematics asked for."

def messages (server: ChatServer) (instructions: String)  : CoreM Json := do
  let main := Json.mkObj [("role", "user"), ("content", instructions)]
  if server.hasSystem then
    let system := Json.mkObj [("role", "system"), ("content", sysPrompt)]
    return Json.arr #[system, main]
  else
    return Json.arr #[main]


end ChatServer

structure ChatExample where
  user : String
  assistant : String

def ChatExample.messages (ex : ChatExample) : List Json :=
  [Json.mkObj [("role", "user"), ("content", ex.user)],
    Json.mkObj [("role", "assistant"), ("content", ex.assistant)]]

abbrev ToChatExample := String × Json → Option ChatExample
