/-!
We have some simple functions `Nat → Nat`
-/

-- <funsearch>
def f1 (n : Nat) : Nat := n + 1
-- </funsearch>

-- <funsearch>
def f2 (n : Nat) : Nat := n * 2
-- </funsearch>

-- <funsearch hard>
def collatz (n: Nat) : Nat := if n % 2 = 0 then n / 2 else 3 * n + 1
-- </funsearch>
