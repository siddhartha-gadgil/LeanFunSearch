import Lean

open Lean Meta Elab Parser PrettyPrinter Term

/-!
Code generation from declarations to include in the search space.
This does not work well for `match` expressions, so it is better to
not use this for LLM sample code.

It can, however, be useful for *tail code* for `loss` and `matchData`.
-/
namespace funsearch

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
