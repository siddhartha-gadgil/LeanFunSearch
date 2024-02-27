import FunSearch.ChatClient
import FunSearch.Frontend
import Lean.Data.Json
open Lean Elab Term

structure FunCode where
  funName: Name
  code: String
  loss: Float
  matchData? : Option Json

def formatLines (lines: List String) : String :=
  lines.foldl (fun acc x => acc ++ "\n" ++ x) ""

namespace FunCode

def instructions (code: FunCode) : List String :=
  let matchLines : List String :=
  match code.matchData? with
    | some (data : Json) =>
      [
        "**Match Data:**",
        data.compress]
    | none => []
  ["", "---",
    "**Loss:**" ++ code.loss.toString] ++
    matchLines ++ [
    "```",
    code.code,
    "```"
  ]

def get? (code: String)(funName: Name) :
  TermElabM <| Except String FunCode := do
  let code := leanBlock code.trim
  let values? ← runDefsViewM? code [funName, `loss, `matchData]
  match values? with
  | Except.error e => return Except.error e
  | Except.ok values =>
    let funName ← getConstInfo funName
    let loss? := values.find? `loss
    let matchData? := values.find? `matchData
    match loss? with
    | none => return Except.error "Expected loss to be defined"
    | some lossString =>
    match parseFloat lossString with
    | Except.error e =>
      return Except.error s!"Error {e} while parsing loss {lossString}"
    | Except.ok loss =>
      let funCode : FunCode :=
      {
        funName := funName.name,
        code := code,
        loss := loss,
        matchData? := matchData?
    }
      return Except.ok funCode

def getAll (codes: Array String)(funName: Name) :
  TermElabM <| (Array FunCode) := do
  let mut funCodes := #[]
  for code in codes do
    let funCode? ← get? code funName
    match funCode? with
    | Except.error e =>
      appendLog "elab_errors" <|
        Json.mkObj [("error", e), ("code", code), ("funName", funName.toString)]
      pure ()
    | Except.ok funCode => funCodes := funCodes.push funCode
  return funCodes

def codeInstructions (funName: Name)
  (codeSamples: List FunCode) : String :=
  let sampleInstructions :=
    codeSamples.bind (fun code => code.instructions)
  let head :=
    s!"Generate code in Lean 4 for the function {funName}\
      to minimize the associated **loss**. \
      \
    Some implementations of the function are given below \
    along with their **loss**. In some cases there is\
    match data indicating expected and actual outputs of some functions\
    depending on the function {funName}."

  formatLines (head :: sampleInstructions)

end FunCode
