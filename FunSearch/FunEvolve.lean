import FunSearch.ChatClient
import FunSearch.Sample
import FunSearch.CodeGen
import FunSearch.Frontend
import FunSearch.FunCode
import FunSearch.Helpers
import Mathlib.Data.Stream.Defs
import Mathlib.Data.LazyList.Basic
import Mathlib.Data.Seq.Seq
open Lean Meta Elab Term

namespace funsearch

structure Evolver extends CodeParams where
  objective: String
  tailCode : String
  server : ChatServer := ChatServer.azure
  params : ChatParams := {n := 3}
  n : Nat := 3
  t : Float := 0.8
  get? : String → String → Name → Name →
    MetaM (Except String FunCode) := FunCode.getLoss?
namespace Evolver

def step (ev: Evolver)(popln : List FunCode) :
    MetaM (List FunCode) := do
  let sample ← pickByLoss popln (fun code ↦ code.loss.toFloat) ev.n ev.t
  let messages :=
    FunCode.messages ev.server ev.objective sample.toArray
  let response ← ev.server.query messages ev.params
  let outputs ←  ChatServer.stringsFromJson response
  let newCodes ←
    FunCode.getAll outputs ev.tailCode ev.funName ev.lossFunction ev.get?
  return newCodes.toList ++ popln |>.eraseDups

def stream (ev: Evolver)(popln : List FunCode) :
    Stream' (MetaM (List FunCode)) :=
    fun n ↦
    match n with
    | 0 => return popln
    | m + 1 => do
      let prev ← stream ev popln m
      let popln ← step ev prev
      return popln

unsafe def lazyList (ev: Evolver)(popln : List FunCode) :
    LazyList (MetaM (List FunCode)) :=
    Stream'.Seq.toLazyList (stream ev popln)

def fromFileSimple (funName : Name)
  (lossFunction: Name := `loss)
  (file: System.FilePath) : MetaM (Evolver × (List FunCode)) := do
  let codes ← funBlocks file
  let tail ← funTailBlock file
  let objective ← funObjectiveBlock file
  let popln ←
      FunCode.getAll codes.toArray tail funName lossFunction
  let ev : Evolver :=
    {objective := objective, funName := funName, tailCode := tail, lossFunction := lossFunction}
  return (ev, popln.toList)

def withNatSample (lo hi n : Nat)
  (funcName eqnName: Name)(file: System.FilePath) :
  MetaM (Evolver × (List FunCode)) := do
  let codes ← funBlocks file
  let objective ← funObjectiveBlock file
  let (tail, pairs) ← tailCodeNat lo hi n funcName eqnName
  let popln ←
      FunCode.getAll codes.toArray tail funcName `loss
        (FunCode.getNatfnDetails? pairs)
  let ev : Evolver :=
    {objective := objective, funName := funcName, tailCode := tail, lossFunction := `loss, get? := FunCode.getNatfnDetails? pairs}
  return (ev, popln.toList)

def withSample (lo hi n : Nat)
  (funcName eqnName ofNat: Name)(file: System.FilePath) :
  MetaM (Evolver × (List FunCode)) := do
  let codes ← funBlocks file
  let objective ← funObjectiveBlock file
  let (tail, pairs) ← tailCodeSample lo hi n funcName eqnName ofNat
  let popln ←
      FunCode.getAll codes.toArray tail funcName `loss
        (FunCode.getSampleDetails? pairs)
  let ev : Evolver :=
    {objective := objective, funName := funcName, tailCode := tail, lossFunction := `loss, get? := FunCode.getSampleDetails? pairs}
  return (ev, popln.toList)

def runAux (ev: Evolver)(popln : List FunCode)
  (steps: Nat)(acceptableLoss : Nat := 0)
  (handle? : Option IO.FS.Handle := none) :
  MetaM (List FunCode) := do
  match handle? with
  | none => pure ()
  | some handle =>
    let js := toJson popln
    handle.putStrLn js.compress
    handle.flush
  if popln.any fun code ↦ code.loss ≤ acceptableLoss then
    return popln.filter fun code ↦ code.loss < acceptableLoss
  else
  match steps with
  | 0 => return popln
  | n + 1 => do
    let popln ← step ev popln
    let minLoss? := popln.map (fun code ↦ code.loss) |>.minimum?
    IO.eprintln "Step completed"
    IO.eprintln s!"minimum loss: {minLoss?}"
    IO.eprintln s!"population: {popln.length}"
    IO.eprintln s!"steps remaining: {steps}"
    runAux ev popln n acceptableLoss handle?

def getPopln? (outFile : System.FilePath) : IO (Option (List FunCode)) := do
  if ← outFile.pathExists then
    let lines ← IO.FS.lines outFile
    let lines := lines.filter fun l => !l.isEmpty
    let js? := lines[lines.size -1]?
    return js?.bind (fun js => (fromJson? js).toOption)
  else
    return none

def run (ev: Evolver)(popln : List FunCode)
    (steps: Nat)
    (outfile? : Option System.FilePath := none)
    (acceptableLoss : Nat := 0) :
    MetaM (List FunCode) := do
  let popln? :=
    (← outfile?.mapM fun out => getPopln? out).join
  let h ←  outfile?.mapM fun f => IO.FS.Handle.mk f .append
  runAux ev (popln?.getD popln) steps acceptableLoss h


end Evolver
