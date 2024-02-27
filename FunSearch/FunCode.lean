import FunSearch.ChatClient
import Lean.Data.Json
open Lean

structure FunCode where
  funName: Name
  code: String
  loss: Float
  matchData? : Option Json

def formatLines (lines: List String) : String :=
  lines.foldl (fun acc x => acc ++ "\n" ++ x) ""

def FunCode.instructions (code: FunCode) : List String :=
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

def codeInstructions (funName: Name)
  (codeSamples: List FunCode) : String :=
  let sampleInstructions :=
    codeSamples.bind (fun code => code.instructions)
  let head :=
    s!"Generate code in Lean 4 to \
      minimize the **loss** for the function {funName}\
      \
    Some implementations of the function are given below \
    along with their **loss** and possibly match data indicating\
    expected and actual outputs of some functions\
    defined in terms of {funName}."

  formatLines (head :: sampleInstructions)
