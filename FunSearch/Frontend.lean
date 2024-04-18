import Lean
import FunSearch.Aides


open Lean Meta Elab Parser
open Std

/-!
Code from Lean 4 copied, simplified and customized. The main change is that instead of parsing the imports the current environment is used. In the entry point `simpleRunFrontend` the environment is passed as an argument.

In the `runFrontendM` and other related functions the environment is modified if the `modifyEnv` flag is set to true.
-/
namespace funsearch

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

def runFrontendM (input: String)(modifyEnv: Bool := false) : CoreM (Environment × MessageLog) := do
  let (env, logs) ← simpleRunFrontend input (← getEnv)
  if modifyEnv then setEnv env
  return (env, logs)

def runDefExprM(s: String)(n: Name)(modifyEnv: Bool := false) : CoreM Expr := do
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
  let prevEnv ← getEnv
  let (env, logs) ← runFrontendM s true
  let pairs : List (Name × Expr) ←
    names.filterMapM <| fun n => do
      let seek? : Option ConstantInfo :=  env.find? n
      match seek? with
      | none => return none
      | some seek => match seek.value? with
        | none => return none
        | some val =>
          -- logInfo m!"Found definition {n}, {← ppExpr val}, {← ppExpr (← reduce val)}"
          let val ← reduce val
          -- logInfo m!"Reduced by ppTerm to {s}"
          let (r,_) ← simp val (← Simp.Context.mkDefault)
          return some (n, r.expr)
  if !modifyEnv then setEnv prevEnv
  return (HashMap.ofList pairs, logs)

def runDefsNatM(s: String)(names: List Name)(modifyEnv: Bool := false) : MetaM (HashMap Name Nat × MessageLog) := do
  let prevEnv ← getEnv
  let (env, logs) ← runFrontendM s true
  let lossExpr? := env.constants.map₁.find? `loss |>.map (·.value!)
  let lf? ← lossExpr?.mapM (ppExpr)
  let ls?' := lf?.map (·.pretty)
  let pairs : List (Name × Nat) ←
    names.mapM <| fun n => do pure (n, ← reduceNatNative n)
  if !modifyEnv then setEnv prevEnv
  -- logInfo m!"Pairs: {pairs}"
  -- let envNames :=
  --   env.constants.map₁.toList.map (·.1) |>.filter (`funsearch).isPrefixOf
  let lossExpr? := env.constants.map₁.find? `loss |>.map (·.value!)
  let lf? ← lossExpr?.mapM (ppExpr)
  let ls? := lf?.map (·.pretty)
  appendLog "losses" <|
    Json.mkObj [("code", s), ("pairs", toJson pairs),
    ("loss", toJson ls?), ("loss'", toJson ls?'),
    -- ("environment-names", toJson envNames),
    ("logs", toJson (← logs.toList.mapM  (·.data.toString)))]
  for log in logs.toList do
    if log.severity == MessageSeverity.error then
      throwError log.data
  logInfo "Checked logs for errors"
  return (HashMap.ofList pairs, logs)

def runDefsBoolM(s: String)(names: List Name)(modifyEnv: Bool := false) : MetaM (HashMap Name Bool × MessageLog) := do
  let prevEnv ← getEnv
  let (_, logs) ← runFrontendM s true
  let pairs : List (Name × Bool) ←
    names.mapM <| fun n => do pure (n, ← reduceBoolNative n)
  if !modifyEnv then setEnv prevEnv
  return (HashMap.ofList pairs, logs)

def runDefsViewM(s: String)(names: List Name)(modifyEnv: Bool := false) : MetaM (HashMap Name String × MessageLog) := do
  let (vals, logs) ← runDefsExprM s names modifyEnv
  let fmts ← vals.toList.mapM fun (n, val) => do
    let fmt ←  ppExpr val
    return (n, fmt.pretty)
  return (HashMap.ofList fmts, logs)

def runDefsViewM? (s: String)(names: List Name)(modifyEnv: Bool := false):
  MetaM <| Except String (HashMap Name String) := do
  -- logInfo m!"Running defs view for {names} in {s}"
  let (vals, logs) ← runDefsViewM s names modifyEnv
  -- logInfo m!"Defs view result: {vals.toList}"
  let mut hasErrors : Bool := false
  let mut l := []
  for msg in logs.toList do
      if msg.severity == MessageSeverity.error then
        let x ← msg.data.toString
        l := l.append [x]
        hasErrors := true
  if hasErrors then
    logInfo m!"Errors found: {l}"
    return Except.error <| "Errors found: " ++ l.toString
  else
    logInfo m!"No errors found, vals : {vals.toList}"
    return Except.ok vals

def checkElabFrontM(s: String) : MetaM <| List String := do
  let (_, log) ← runFrontendM s
  let mut l := []
  for msg in log.toList do
    if msg.severity == MessageSeverity.error then
      let x ← msg.data.toString
      l := l.append [x]
  return l

def egNat := 3

def egNatNative : MetaM Nat := do
  let n ← reduceNatNative ``egNat
  return n

#eval egNatNative

end funsearch
