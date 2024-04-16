import FunSearch.ChatClient
import FunSearch.Sample
import FunSearch.CodeGen
import FunSearch.Frontend
import FunSearch.FunCode
import FunSearch.Helpers
open Lean Meta Elab Term

namespace funsearch

structure EvolveParams extends CodeParams where
  objective: String
  tailCode : String
  server : ChatServer := ChatServer.azure
  params : ChatParams := {n := 3}
  n : Nat := 3
  t : Float := 0.8
  matchData : Bool := false

def evolveStep (ev: EvolveParams)(popln : List FunCode) :
    TermElabM (List FunCode) := do
  let sample ← pickByLoss popln (fun code ↦ code.loss) ev.n ev.t
  let messages :=
    FunCode.messages ev.server ev.objective ev.funName sample.toArray
  let response ← ev.server.query messages ev.params
  let outputs ←  ChatServer.stringsFromJson response
  let newCodes ←
    FunCode.getAll outputs ev.tailCode ev.funName ev.lossFunction ev.lossDetails?
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

def runEvolution (objective: String)(funName : Name)
  (lossFunction: Name := `loss)
  (lossDetails? : Option Name := none)
  (file: System.FilePath)(steps: Nat := 100)
  (acceptableLoss : Float := 0.0) : TermElabM (List FunCode) := do
  let codes ← funBlocks file
  let tail ← funTailBlock file
  let popln ←
      FunCode.getAll codes.toArray tail funName lossFunction lossDetails?
  let ev : EvolveParams :=
    {objective := objective, funName := funName, tailCode := tail, lossFunction := lossFunction, lossDetails? := lossDetails?}
  evolution ev popln.toList steps acceptableLoss
