import FunSearch.CodeGen
import FunSearch.FunCode

/-!
We have some simple functions `Nat → Nat`

## FunObjective

Find a function `fn: Nat → Nat` that satisfies $f(n+ 1)^2 = f(n)^2 + 4n + 8$.

## EndObjective


-/
namespace eg1

-- <funsearch>
def fn (n : Nat) : Nat := n + 1
-- </funsearch>

end eg1

namespace eg2
-- <funsearch>
def fn (n : Nat) : Nat := n * 2
-- </funsearch>
end eg2

namespace collatz

-- <funsearch hard>
def fn (n: Nat) : Nat := if n % 2 = 0 then n / 2 else 3 * n + 1
-- </funsearch>

end collatz

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

end funsearch
