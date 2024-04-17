import FunSearch.ChatClient
import FunSearch.Frontend
import Lean.Data.Json
open Lean Elab Term

namespace funsearch

instance : Repr Json where
  reprPrec json _ := json.compress

structure FunCode where
  funName: Name
  code: String
  loss: Nat
  matchData? : Option Json
  deriving BEq, Repr

def formatLines (lines: List String) : String :=
  lines.foldl (fun acc x => acc ++ "\n" ++ x) ""

namespace FunCode

def goal (objective: String) : String :=
  s!"{objective}.\n\nThe extent to which the objective is satisfied will be measured by a **loss** function, which you must minimize.\n\nGive ONLY the code in Lean."

def report (funCode: FunCode) (objective: String) : String :=
  let loss := s!"The code you gave was to minimize the loss of the function {funCode.funName}, which measured how well the function satisfied:\n {objective}. \n\n## Loss: For the code you gave, the **loss** was {funCode.loss}."
  let details := funCode.matchData?.map (fun matchData =>
    s!"\n\n## Match Data: More details on the terms of the loss for the function {funCode.funName} are as follows. \n\n{matchData.compress}") |>.getD ""
  loss ++ details ++ "\n---\n"

def simpleInstructions (code: FunCode) : List String :=
  let matchLines : List String :=
  match code.matchData? with
    | some (data : Json) =>
      [
        "**Match Data:**",
        data.compress]
    | none => []
  ["", "---",
    "**Loss:**" ++ (toString code.loss)] ++
    matchLines ++ [
    "```",
    code.code,
    "```"
  ]

def getLoss? (code: String)(tailCode: String)(funName lossFunction: Name) :
  MetaM <| Except String FunCode := do
  let fullCode := leanBlock code.trim ++ "\n\n" ++ tailCode
  -- logInfo fullCode
  -- logInfo m!"{names}"
  try
    let (values, logs) ← runDefsNatM fullCode [`loss]
    for msg in logs.toList do
      if msg.severity == MessageSeverity.error then
        logWarning msg.data
    let loss? := values.find? lossFunction
    match loss? with
    | none => return Except.error "Expected loss to be defined"
    | some lossNat =>
      let funCode : FunCode :=
      {
        funName := funName,
        code := code,
        loss := lossNat,
        matchData? := none
      }
      -- logInfo m!"{funCode.code}, {funCode.loss}, {funCode.matchData?}"
      return Except.ok funCode
  catch e => return Except.error (← e.toMessageData.toString)

def getNatfnDetails? (evalPoints: List (Name × Nat)) (code: String)(tailCode: String)(funName lossFunction: Name) :
  MetaM <| Except String FunCode := do
  let fullCode := leanBlock code.trim ++ "\n\n" ++ tailCode
  let pairs := evalPoints
  let names := pairs.map (·.1)
  -- logInfo fullCode
  -- logInfo m!"{names}"
  try
    let (values, logs) ← runDefsNatM fullCode (`loss :: names)
    for msg in logs.toList do
      if msg.severity == MessageSeverity.error then
        logWarning msg.data
    let loss? := values.find? lossFunction
    match loss? with
    | none => return Except.error "Expected loss to be defined"
    | some lossNat =>
      let pointErrors := pairs.filterMap <| fun (name, nat) =>
        let value? := values.find? name
        value?.map (fun value =>
          Json.mkObj [("point", nat), ("error", value)])
      let funCode : FunCode :=
      {
        funName := funName,
        code := code,
        loss := lossNat,
        matchData? := some <| Json.arr pointErrors.toArray
      }
      -- logInfo m!"{funCode.code}, {funCode.loss}, {funCode.matchData?}"
      return Except.ok funCode
  catch e => return Except.error (← e.toMessageData.toString)

def getAll (codes: Array String)(tailCode: String)
  (funName lossFunction : Name)
  (get? : String → String → Name → Name →
    MetaM (Except String FunCode) := getLoss?) :
  MetaM <| (Array FunCode) := do
  let mut funCodes := #[]
  for code in codes do
    let funCode? ←
      get? code tailCode funName lossFunction
    match funCode? with
    | Except.error e =>
      appendLog "elab_errors" <|
        Json.mkObj [("error", e), ("code", code), ("funName", funName.toString), ("tailCode", tailCode)]
      logError <| "e" ++ "\nin" ++ code ++ "\n\n" ++ tailCode
      pure ()
    | Except.ok funCode => funCodes := funCodes.push funCode
  return funCodes


def getAllNatFn (codes: Array String)(tailCode: String)(evalPoints: List (Name × Nat))
  (funName lossFunction : Name) :
  MetaM <| (Array FunCode) :=
  getAll codes tailCode funName lossFunction
   (getNatfnDetails? evalPoints)


def getAllIO (codes: IO (List String))
    (tailCode: MetaM (String × List (Name × Nat)))
    (funName : Name) (lossFunction : Name := `loss) :
    MetaM (Array FunCode) := do
  let codes ← codes
  getAllNatFn codes.toArray (← tailCode).1 (← tailCode).2 funName lossFunction

def messages (server: ChatServer)(objective: String)
  (funCodes: Array FunCode) : Json :=
  let goalMessage := goal objective
  let (msgs, report?) := funCodes.foldl (fun (acc, report?) funCode =>
    let prevReport :=
      report?.getD ""
    let acc := acc ++
      #[Json.mkObj [("role", "user"), ("content", prevReport ++ goalMessage), ("role", "assistant"), ("content", funCode.code)]]
    (acc, some <| report funCode objective))
    (server.sysMessage, none)
  let prevReport :=
      report?.getD ""
  let messages := msgs ++
    #[Json.mkObj [("role", "user"), ("content", prevReport ++ goalMessage)]]
  Json.arr messages


def simpleCodeInstructions (funName: Name)
  (codeSamples: List FunCode)(matchData: Bool := false)
  (properties: List String := []) : String :=
  let sampleInstructions :=
    codeSamples.bind (fun code => code.simpleInstructions)
  let head :=
    s!"Generate code in Lean 4 for the function {funName}\
      to minimize the associated **loss**. \
      \
    Some implementations of the function are given below \
    along with their **loss**."
  let head :=
    if matchData then
      head ++
      " The match data gives expected and actual outputs of some functions\
       that depend on the function {funName}."
    else head
  formatLines (head :: properties ++ sampleInstructions)

/-
TODO: Have a collection of messages instead of a single message.
-/

end FunCode
