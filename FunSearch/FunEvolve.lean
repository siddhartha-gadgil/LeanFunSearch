import FunSearch.ChatClient
import FunSearch.Sample
import FunSearch.CodeGen
import FunSearch.Frontend
import FunSearch.FunCode
open Lean Meta Elab Term

namespace funsearch

structure EvolveParams where
  funName : Name
  tailCode : String
  server : ChatServer := ChatServer.azure
  params : ChatParams := {}
  n : Nat := 3
  t : Float := 0.8
  matchData : Bool := false
  properties : List String := []


def evolveStep (ev: EvolveParams)(popln : List FunCode) :
    TermElabM (List FunCode) := do
  let sample ← pickByLoss popln (fun code ↦ code.loss) ev.n ev.t
  let instructions :=
    FunCode.codeInstructions ev.funName sample ev.matchData ev.properties
  let outputs ← ev.server.queryTexts instructions ev.params
  let newCodes ← FunCode.getAll outputs ev.tailCode ev.funName
  return newCodes.toList ++ popln |>.eraseDups


def evolution (ev: EvolveParams)(popln : List FunCode)
  (steps: Nat)(acceptableLoss : Float := 0.0) :
  TermElabM (List FunCode) := do
  if popln.any fun code ↦ code.loss < acceptableLoss then
    return popln.filter fun code ↦ code.loss < acceptableLoss
  else
  match steps with
  | 0 => return popln
  | n + 1 => do
    let popln ← evolveStep ev popln
    let minLoss? := popln.map (fun code ↦ code.loss) |>.minimum?
    IO.println "Step completed"
    IO.println s!"minimum loss: {minLoss?}"
    IO.println s!"population: {popln.length}"
    IO.println s!"steps remaining: {steps}"
    evolution ev popln n acceptableLoss

def runEvolution (funName : Name)(file: System.FilePath)(steps: Nat := 100)
  (acceptableLoss : Float := 0.0) : TermElabM (List FunCode) := do
  let codes ← funBlocks file
  let tail ← funTailBlock file
  let popln ← FunCode.getAll codes.toArray tail funName
  let ev : EvolveParams := {funName := funName, tailCode := tail}
  evolution ev popln.toList steps acceptableLoss
