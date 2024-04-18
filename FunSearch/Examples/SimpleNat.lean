import FunSearch.CodeGen
import FunSearch.FunCode
import FunSearch.FunEvolve
import Mathlib

/-!
We have some simple functions `Nat → Nat`

## FunObjective

Find a function `fn: Nat → Nat` that satisfies $f(n+ 1)^2 = f(n)^2 + 4n + 8$.

## EndObjective


-/
namespace eg1

-- start-funsearch
def fn (n : Nat) : Nat := n + 1
-- end-funsearch

end eg1

namespace eg2
-- start-funsearch
def fn (n : Nat) : Nat := n * 2
-- end-funsearch
end eg2

namespace collatz

-- start-funsearch hard
def fn (n: Nat) : Nat := if n % 2 = 0 then n / 2 else 3 * n + 1
-- end-funsearch

end collatz

namespace recursive

-- start-funsearch
def fn : Nat → Nat
| 0 => 3
| n + 1 => fn n + 2
-- end-funsearch

end recursive

namespace funsearch

def fnEqn (f: Nat → Nat) : Nat → Float :=
  fun n ↦ (f (n + 1) * f (n + 1) - (f (n) * f (n)) - 4 * n - 4).toFloat

def fnEqnNat (f: Nat → Nat) : Nat → Nat :=
  fun n ↦ Int.natAbs
    (f (n + 1) * f (n + 1) : Int) - ((f (n) * f (n)) - 4 * n - 8)


def codeHeads : IO <| List String :=
  funBlocks "FunSearch/Examples/SimpleNat.lean"

def objective := funObjectiveBlock "FunSearch/Examples/SimpleNat.lean"

#eval codeHeads

#eval objective

def tail := tailCodeNat 1 100 7 `fn ``fnEqnNat

#eval tail

def startCode := FunCode.getAllIO codeHeads tail `fn
#eval startCode

open Lean Meta Elab
def msgs : MetaM Json := do
  let server := ChatServer.azure
  let msgs :=
    FunCode.messages server
      (← objective) (← startCode)
  return msgs

#eval msgs

def response : MetaM Json := do
  let server := ChatServer.azure
  let msgs :=
    FunCode.messages server
      (← objective) (← startCode)
  let response ← server.query msgs {n := 3}
  return response

-- #eval response

def egEvolve := Evolver.withNatSample 0 100 12 `fn ``fnEqnNat "FunSearch/Examples/SimpleNat.lean"

def egObj : MetaM String := do
  return (← egEvolve).1.objective

def egTailCode : MetaM String := do
  return (← egEvolve).1.tailCode

def egPop : MetaM (List FunCode) := do
  let (_, pop) := (← egEvolve)
  return pop

#eval egPop

#check egEvolve
#eval egObj
#eval egTailCode

def egStep : MetaM (List FunCode) := do
  let (ev, pop) := (← egEvolve)
  ev.step pop

-- #eval egStep

end funsearch
