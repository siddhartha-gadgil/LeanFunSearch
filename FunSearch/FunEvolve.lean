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
  return newCodes.toList ++ popln


def evolution (ev: EvolveParams)(popln : List FunCode)
  (steps: Nat)(acceptableLoss : Float) :
  TermElabM (List FunCode) := do
  if popln.any fun code ↦ code.loss < acceptableLoss then
    return popln.filter fun code ↦ code.loss < acceptableLoss
  else
  match steps with
  | 0 => return popln
  | n + 1 => do
    let popln ← evolveStep ev popln
    evolution ev popln n acceptableLoss
