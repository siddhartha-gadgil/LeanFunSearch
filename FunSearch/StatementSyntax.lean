import Lean

open Lean Meta Elab Parser PrettyPrinter Term

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
