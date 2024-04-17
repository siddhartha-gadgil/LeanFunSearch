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
def sampleNatStx (lo hi n: Nat)(sampleFn: Name) :
    CoreM ((TSyntax `command) × (Syntax.Term) × (List Nat)) := do
  let sample ←  List.replicate n 0 |>.mapM fun _ =>
    IO.rand lo hi
  let s := sample.toString
  let stx? := Parser.runParserCategory (← getEnv) `term s
  match stx? with
  | Except.ok stx =>
    let stx : TSyntax `term := ⟨ stx ⟩
    let lst := mkIdent `List
    let nat := mkIdent `Nat
    let sampleId := mkIdent sampleFn
    let result ← `(command|def $sampleId : $lst $nat := $stx)
    return (result, stx, sample)
  | Except.error err =>
    throwError m!"{err}"


def sampleNats (lo hi n: Nat)(sampleFn: Name) : CoreM (Format × (List Nat)) := do
    let (stx, _, sample) ← sampleNatStx lo hi n sampleFn
    return (← ppCommand stx, sample)

#eval (sampleNats 0 100 10 `sample)

def sampleα (lo hi n: Nat)(sampleFn fromNat: Name) :
        CoreM (Format × (List Syntax.Term)) := do
    let (_, rhs, sampleNat) ← sampleNatStx lo hi n sampleFn
    let fromNatId := mkIdent fromNat
    let sample ← sampleNat.mapM fun n => do
        let n := Syntax.mkNumLit (toString n)
        `($fromNatId $n)
    let sampleId := mkIdent sampleFn
    let cmd ← `(command|def $sampleId  := List.map $fromNatId $rhs)
    return (← ppCommand cmd, sample)

/--
## Tail code for Nat → Nat functions: Broken version

We have the function name, equation name and the sampling parameters as arguments.

We will use the default names `loss` and `lossDetails` for the loss and the details. These will use functions from the helper.

The equation is expected to take a single argument, the function. So the equation is `eqnName funcName`.

We can generalize this to functions `α → β` by using the `sampleα` function for the `sampleFmt` argument and an appropriate absolute value function for `absFn`.
-/
def sampleα' [ToString α] (lo hi n: Nat)(sampleFn: Name)(m : Nat → α) : CoreM (Format) := do
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

#eval (sampleα'  0 100 10 `sample Nat.toFloat)



def tailCodeFmt' (lo hi n : Nat)(funcName eqnName: Name)
    (sampleFmt : CoreM (Format × List Nat) := sampleNats lo hi n `sample)
    (absFn : Name := ``natAbs):
    CoreM Format := do
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
        `(command| def $lossFn := $sampleLossFn $eqn $funcId $m $sample)
    let lossDetailsStx ←
        `(command| def $lossDetailsFn := $sampleLossDetailsFn $eqn $funcId $m $sample)
    let fmt := Format.joinSep
        [(← sampleFmt).1, ← ppCommand lossStx, ← ppCommand lossDetailsStx] (Format.line ++ Format.line)
    return fmt

def tailCode'(lo hi n : Nat)(funcName eqnName: Name)
    (sampleFmt : CoreM (Format × List Nat) := sampleNats lo hi n `sample)
    (absFn : Name := ``natAbs):
    CoreM String := do
    let fmt ← tailCodeFmt' lo hi n funcName eqnName sampleFmt absFn
    return fmt.pretty

/--
## Tail code for Nat → Nat functions

We have the function name, equation name and the sampling parameters as arguments.

We will use the default names `loss` and `lossDetails` for the loss and the details. These will use functions from the helper.

The equation is expected to take a single argument, the function. So the equation is `eqnName funcName`.

We can generalize this to functions `α → β` by using the `sampleα` function for the `sampleFmt` argument and an appropriate absolute value function for `absFn`.
-/
def tailCodeNatFmt (lo hi n : Nat)(funcName eqnName: Name)
    (sampleFmt : CoreM (Format × List Nat) := sampleNats lo hi n `sample):
    CoreM (Format × List (Name × Nat)) := do
    let sampleId := mkIdent `sample
    let lossFn := mkIdent `loss
    let eqnId := mkIdent eqnName
    let funcId := mkIdent funcName
    let sampleLossFn := mkIdent ``sampleLossNat
    let eqn ← `($eqnId $funcId)
    let nat := mkIdent `Nat
    let lossStx ←
        `(command| def $lossFn : $nat := $sampleLossFn $eqn $sampleId)
    let lossFmt ← ppCommand lossStx
    let mut commands := #[(← sampleFmt).1, lossFmt]
    let mut namePairs : Array (Name × Nat) := #[]
    let mut i := 0
    for j in (← sampleFmt).2 do
        let name := s!"lossDataPoint_{i}".toName
        let nameId := mkIdent name
        namePairs := namePairs.push (name, j)
        let jstx := Syntax.mkNumLit (toString j)
        let cmd ← `(command| def $nameId : $nat := $eqn $jstx)
        commands := commands.push (← ppCommand cmd)
        i := i + 1
    let fmt := Format.joinSep
        commands.toList (Format.line ++ Format.line)
    return (fmt, namePairs.toList)

def tailCodeNat (lo hi n : Nat)(funcName eqnName: Name)
    (sampleFmt : CoreM (Format × List Nat) := sampleNats lo hi n `sample):
    CoreM (String × List (Name × Nat)) := do
    let (fmt, pairs) ← tailCodeNatFmt lo hi n funcName eqnName sampleFmt
    return (fmt.pretty, pairs)

#eval tailCodeNat 1 100 7 `fn `fnEqn

def tailCodeSampleFmt (lo hi n : Nat)(funcName eqnName ofNat: Name)
    (sampleFmt : CoreM (Format × List Syntax.Term) :=
        sampleα  lo hi n `sample ofNat):
    CoreM (Format × List (Name × Format)) := do
    let sampleId := mkIdent `sample
    let lossFn := mkIdent `loss
    let eqnId := mkIdent eqnName
    let funcId := mkIdent funcName
    let sampleLossFn := mkIdent ``sampleLossNat
    let eqn ← `($eqnId $funcId)
    let nat := mkIdent `Nat
    let lossStx ←
        `(command| def $lossFn : $nat := $sampleLossFn $eqn $sampleId)
    let lossFmt ← ppCommand lossStx
    let mut commands := #[(← sampleFmt).1, lossFmt]
    let mut namePairs : Array (Name × Format) := #[]
    let mut i := 0
    for jval in (← sampleFmt).2 do
        let name := s!"lossDataPoint_{i}".toName
        let nameId := mkIdent name
        let p ← PrettyPrinter.ppTerm jval
        namePairs := namePairs.push (name, p)
        let cmd ← `(command| def $nameId : $nat := $eqn $jval)
        commands := commands.push (← ppCommand cmd)
        i := i + 1
    let fmt := Format.joinSep
        commands.toList (Format.line ++ Format.line)
    return (fmt, namePairs.toList)

def tailCodeSample (lo hi n : Nat)(funcName eqnName ofNat: Name)
    (sampleFmt : CoreM (Format × List Syntax.Term) :=
        sampleα  lo hi n `sample ofNat):
    CoreM (String × List (Name × Format)) := do
    let (fmt, pairs) ← tailCodeSampleFmt lo hi n funcName eqnName ofNat sampleFmt
    return (fmt.pretty, pairs)

#eval tailCodeSample 1 100 7 `fn `fnEqn ``Int.ofNat


/-!
Other code generation. Currently not used.
-/
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
