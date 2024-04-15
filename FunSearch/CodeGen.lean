import Lean
import FunSearch.Helpers

open Lean Meta Elab Parser PrettyPrinter Term

/-!
Code generation for the tailcode.

Also, code generation from declarations to include in the search space.
This does not work well for `match` expressions, so it is better to
not use this for LLM sample code.

It can, however, be useful for *tail code* for `loss` and `matchData`.
-/
namespace funsearch

/--
## Sampling natural numbers

Sampling natural numbers. To sample other types, we can map from natural numbers.
-/
def sampleNats (lo hi n: Nat)(sampleFn: Name) : MetaM (Format) := do
  let sample ←  List.replicate n 0 |>.mapM fun _ =>
    IO.rand lo hi
  let s := sample.toString
  let stx? := Parser.runParserCategory (← getEnv) `term s
  match stx? with
  | Except.ok stx =>
    let stx : TSyntax `term := ⟨ stx ⟩
    let lst := mkIdent `List
    let nat := mkIdent `Nat
    let sample := mkIdent sampleFn
    let result ← `(command|def $sample : $lst $nat := $stx)
    ppCommand result
  | Except.error err =>
    throwError m!"{err}"

#eval (sampleNats 0 100 10 `sample)

def sampleα [ToString α] (lo hi n: Nat)(sampleFn: Name)(m : Nat → α) : MetaM (Format) := do
  let sample ←  List.replicate n 0 |>.mapM fun _ => do
    pure <| m (← IO.rand lo hi)
  let s := sample.toString
  let stx? := Parser.runParserCategory (← getEnv) `term s
  match stx? with
  | Except.ok stx =>
    let stx : TSyntax `term := ⟨ stx ⟩
    let lst := mkIdent `List
    let nat := mkIdent `Nat
    let sample := mkIdent sampleFn
    let result ← `(command|def $sample : $lst $nat := $stx)
    ppCommand result
  | Except.error err =>
    throwError m!"{err}"

#eval (sampleα  0 100 10 `sample Nat.toFloat)

/--
## Tail code for Nat → Nat functions

We have the function name, equation name and the sampling parameters as arguments.

We will use the default names `loss` and `lossDetails` for the loss and the details. These will use functions from the helper.

The equation is expected to take a single argument, the function. So the equation is `eqnName funcName`.

We can generalize this to functions `α → β` by using the `sampleα` function for the `sampleFmt` argument and an appropriate absolute value function for `absFn`.
-/
def tailCode (lo hi n : Nat)(funcName eqnName: Name)
    (sampleFmt : MetaM Format := sampleNats lo hi n `sample)
    (absFn : Name := ``natAbs):
    MetaM Format := do
    let sample := mkIdent `sample
    let lossFn := mkIdent `loss
    let eqnId := mkIdent eqnName
    let funcId := mkIdent funcName
    let sampleLossFn := mkIdent ``sampleLoss
    let lossDetailsFn := mkIdent `lossDetails
    let sampleLossDetailsFn := mkIdent ``sampleLossDetails
    let eqn ← `($eqnId $funcId)
    let m := mkIdent absFn
    let lossStx ←
        `(command| def $lossFn := $sampleLossFn $eqn $m $sample)
    let lossDetailsStx ←
        `(command| def $lossDetailsFn := $sampleLossDetailsFn $eqn $m $sample)
    let fmt := Format.joinSep
        [← sampleFmt, ← ppCommand lossStx, ← ppCommand lossDetailsStx] (Format.line ++ Format.line)
    return fmt

#eval tailCode 1 100 7 `fn `fnEqn

#eval (Float.abs ∘ Nat.toFloat) 3

partial def arrowHeads (type: Syntax.Term)
    (accum: Array <| TSyntax ``bracketedBinder := #[]) :
        CoreM <| (Array <| TSyntax ``bracketedBinder) × Syntax.Term := do
    match type with
    | `(depArrow|$bb → $body) => do
        let accum := accum.push bb
        arrowHeads body accum
    | _ => return (accum, type)


def mkStatementStx (name: Name)(type: Syntax.Term)
    (value?: Option Syntax.Term)(isProp: Bool) :
        CoreM (TSyntax `command) := do
    let (ctxs, tailType) ← arrowHeads type
    let value := value?.getD (← `(by sorry))
    let name := mkIdent name
    if isProp
    then
        `(command| theorem $name $ctxs* : $tailType := $value)
    else
        `(command| def $name:ident $ctxs* : $tailType := $value)

def mkStatement (name: Name)(type: Syntax.Term)
    (value?: Option Syntax.Term)(isProp: Bool) :
        CoreM String := do
    let stx ← mkStatementStx name type value? isProp
    let fmt ← ppCategory `command stx
    return fmt.pretty

#check Meta.isProp

def declDefnStx (declName : Name)(name: Name := declName):
        MetaM (TSyntax `command) := do
    let info ← getConstInfo declName
    let type ← instantiateMVars info.type
    let value? ←  info.value?.mapM fun v => instantiateMVars v
    let typeStx ← delab type
    let valueStx? ← value?.mapM fun v => delab v
    let isProp ← isProp type
    mkStatementStx name typeStx valueStx? isProp

def declDefn (declName : Name)(name: Name := declName):
        MetaM String := do
    let stx ← declDefnStx declName name
    let fmt ← ppCategory `command stx
    return fmt.pretty

set_option pp.match true

#eval declDefn `Nat.add
#print Nat.add

def fn (n: Nat) := match n with
    | 0 => 1
    | n + 1 => n * fn n

#print fn

#eval declDefn ``fn

def fn' (n: Nat) := n + 1

#eval declDefn ``fn'
#print fn'

end funsearch
