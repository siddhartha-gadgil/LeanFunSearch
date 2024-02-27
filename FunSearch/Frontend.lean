import Lean


open Lean Meta Elab Parser
open Std

/-!
Code from Lean 4 copied, simplified and customized. The main change is that instead of parsing the imports the current environment is used. In the entry point `simpleRunFrontend` the environment is passed as an argument.

In the `runFrontendM` and other related functions the environment is modified if the `modifyEnv` flag is set to true.
-/

def simpleRunFrontend
    (input : String)
    (env: Environment)
    (opts : Options := {})
    (fileName : String := "<input>")
    : IO (Environment × MessageLog) := do
  let inputCtx := Parser.mkInputContext input fileName
  let commandState := Command.mkState env (opts := opts)
  let parserState: ModuleParserState := {}
  let s ← IO.processCommands inputCtx parserState commandState
  pure (s.commandState.env, s.commandState.messages)

def runFrontendM (input: String)(modifyEnv: Bool := false) : MetaM (Environment × MessageLog) := do
  let (env, logs) ← simpleRunFrontend input (← getEnv)
  if modifyEnv then setEnv env
  return (env, logs)

def runDefExprM(s: String)(n: Name)(modifyEnv: Bool := false) : MetaM Expr := do
  let (env, _) ← runFrontendM s modifyEnv
  let seek? : Option ConstantInfo :=  env.find? n
  match seek? with
  | none => throwError "Definition not found"
  | some seek => match seek.value? with
    | none => throwError "Definition has no value"
    | some val => return val

def runDefViewM(s: String)(n: Name)(modifyEnv: Bool := false) : MetaM String := do
  let val ← runDefExprM s n modifyEnv
  let fmt ←  ppExpr val
  return fmt.pretty

def runDefsExprM(s: String)(names: List Name)(modifyEnv: Bool := false) : MetaM (HashMap Name Expr × MessageLog) := do
  let (env, logs) ← runFrontendM s modifyEnv
  let pairs : List (Name × Expr) ←
    names.filterMapM <| fun n => do
      let seek? : Option ConstantInfo :=  env.find? n
      match seek? with
      | none => return none
      | some seek => match seek.value? with
        | none => return none
        | some val => return some (n, val)
  return (HashMap.ofList pairs, logs)

def runDefsViewM(s: String)(names: List Name)(modifyEnv: Bool := false) : MetaM (HashMap Name String × MessageLog) := do
  let (vals, logs) ← runDefsExprM s names modifyEnv
  let fmts ← vals.toList.mapM fun (n, val) => do
    let fmt ←  ppExpr val
    return (n, fmt.pretty)
  return (HashMap.ofList fmts, logs)

def runDefsViewM? (s: String)(names: List Name)(modifyEnv: Bool := false):
  MetaM <| Except String (HashMap Name String) := do
  let (vals, logs) ← runDefsViewM s names modifyEnv
  let mut hasErrors : Bool := false
  let mut l := []
  for msg in logs.toList do
      if msg.severity == MessageSeverity.error then
        let x ← msg.data.toString
        l := l.append [x]
        hasErrors := true
  if hasErrors then
    return Except.error <| "Errors found: " ++ l.toString
  else
    return Except.ok vals

def checkElabFrontM(s: String) : MetaM <| List String := do
  let (_, log) ← runFrontendM s
  let mut l := []
  for msg in log.toList do
    if msg.severity == MessageSeverity.error then
      let x ← msg.data.toString
      l := l.append [x]
  return l
