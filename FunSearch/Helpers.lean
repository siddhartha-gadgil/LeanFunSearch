import Lean.Data.Json
open Lean

/-!
Helper functions for running with generated code. Should be imported in the environment where the code is elaborated.
-/
namespace funsearch

variable {α : Type}{β : Type}

/--
Loss by averaging the error given by `eqn` over the sample, normalized by typically the absolute value.
-/
def sampleLoss (eqn: α → Float)(f : α → β)(m : β  → Float)(sample : List α) : Nat :=
  let n := sample.length
  let sum := sample.foldl (init := 0.0)
    (fun acc x => acc + (eqn x / (1.0 + m (f x))))
  (sum * 100 / n.toFloat) |>.toUSize |>.toNat

def sampleLossNat (eqn: α → Nat)(sample : List α) : Nat :=
  sample.foldl (init := 0)
    (fun acc x => acc + eqn x)

def sampleLossDetails [ToJson α](eqn: α → Float)(f : α → β)(m : β  → Float)
  (sample : List α) : Json :=
  let obs := (
    sample.map (fun x =>
      let error := eqn x
      let loss := error / (1.0 + m (f x))
      Json.mkObj [
        ("x", toJson x),
        ("equation-error", toJson error),
        ("loss", toJson loss)]
    )
  )
  Json.arr obs.toArray

def natAbs : Nat → Float := Float.abs ∘ Nat.toFloat

#check Nat.toFloat

structure CodeParams where
  funName : Name
  lossFunction: Name := `loss
  lossDetails? : Option Name := some `lossDetails
