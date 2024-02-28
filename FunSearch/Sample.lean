namespace funsearch

def normalize (l: List (α × Float)) : List (α × Float) :=
  let sum := (l.map (fun (_, w) => w)).foldl (· + ·)  0.0
  l.map (fun (x, w) => (x, w / sum))

def softMax [BEq α](l: List α)(loss: α → Float)(t: Float) : List (α × Float) :=
  if t > 0 then
    let exps := l.map (fun x =>
      (- (loss x) / t).exp)
    let sum := exps.foldl (· + ·) 0.0
    l.map (fun x => (x, (- (loss x) * t).exp / sum))
  else
    match (l.map (fun x => loss x)).maximum? with
    | none => []
    | some maxLoss =>
      let tops := l.filter (fun x => loss x == maxLoss)
      tops.map (fun x => (x, 1.0 / tops.length.toFloat))

def pickElem (l: List (α × Float))(r: Float) : IO α :=
  match l with
  | [] => throw <| IO.userError s!"picking weight {r} from empty list"
  | (x, w) :: tail =>
    if r < w then
      pure x
    else
      pickElem tail (r - w)

def pickElemRec? (l: List (α × Float))(r: Float)(skipped : List (α × Float)) :
    Option <| α × List (α × Float) :=
  match l with
  | [] => none
  | (x, w) :: tail =>
    if r < w then
      pure (x, skipped ++ tail)
    else
      pickElemRec? tail (r - w) (skipped ++ [(x, w)])

def pickElems (l: List (α × Float))(n: Nat) : IO (List α) :=
  match n with
  | 0 => pure []
  | m + 1 => do
    let r1000 ← IO.rand 0 999
    let r := r1000.toFloat / 1000.0
    match pickElemRec? l r [] with
    | none => pure []
    | some (x, remaining) => do
      let xs ← pickElems (normalize remaining) m
      pure (x :: xs)

def pickByLoss [BEq α](l : List α)(loss: α → Float)(n: Nat)(t: Float) :
  IO (List α) := do
  pickElems (softMax l loss t) n

end funsearch

open funsearch

def eg :=
  let l := [(1, 0.1), (2, 0.2), (3, 0.3), (4, 0.4)]
  let l' := normalize l
  pickElemRec? l' 0.5 []


#eval eg

#eval pickElems [(1, 0.2), (2, 0.2), (3, 0.3), (4, 0.4)] 2

#eval List.range 20 |>.mapM
  (fun _ => pickElems (normalize [(1, 0.2), (2, 0.2), (3, 0.3), (4, 0.4)]) 2)
