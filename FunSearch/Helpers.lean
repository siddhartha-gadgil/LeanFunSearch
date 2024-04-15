import Lean.Data.Json
open Lean

/-!
Helper functions for running with generated code. Should be imported in the environment where the code is elaborated.
-/
namespace funsearch

variable {α : Type}

/--
Loss by averaging the error given by `eqn` over the sample, normalized by typically the absolute value.
-/
def sampleLoss (eqn: α → Float)(m : α → Float)(sample : Array α) : Float :=
  let n := sample.size
  let sum := sample.foldl (init := 0.0)
    (fun acc x => acc + (eqn x / (1.0 + m x)))
  sum / n.toFloat

def sampleLossDetails [ToJson α](eqn: α → Float)(m : α → Float)
  (sample : Array α) : Json :=
  let obs := (
    sample.map (fun x =>
      let error := eqn x
      let loss := error / (1.0 + m x)
      Json.mkObj [
        ("x", toJson x),
        ("equation-error", toJson error),
        ("loss", toJson loss)]
    )
  )
  Json.arr obs

def natAbs : Nat → Float := Float.abs ∘ Nat.toFloat

#check Nat.toFloat

structure CodeParams where
  funName : Name
  lossFunction: Name := `loss
  lossDetails? : Option Name := some `lossDetails
