import FunSearch.CodeGen
import FunSearch.FunCode

/-!
We have some simple functions `Nat → Nat`
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

#eval codeHeads

def tail := tailCodeNat 1 100 7 `fn ``fnEqnNat

#eval tail

#check FunCode.getAllIO codeHeads tail `fn

#check Lean.Name.num
#eval FunCode.getAllIO codeHeads tail `fn


end funsearch
