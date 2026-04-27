module CNFConversion

open Logic

type Literal =
    | Pos of string
    | Neg of string

type Clause = Set<Literal>
type CNF = Set<Clause>

// Helpers for breaking down the formula
let rec eliminateBiImplies (formula: Formula) : Formula =
    match formula with
    | BiImplies (p, q) -> And (Implies (eliminateBiImplies p, eliminateBiImplies q), 
                               Implies (eliminateBiImplies q, eliminateBiImplies p))
    | And (p, q)       -> And (eliminateBiImplies p, eliminateBiImplies q)
    | Or (p, q)        -> Or  (eliminateBiImplies p, eliminateBiImplies q)
    | Implies (p, q)   -> Implies (eliminateBiImplies p, eliminateBiImplies q)
    | Not p            -> Not (eliminateBiImplies p)
    | f                -> f

let rec eliminateImplies (formula: Formula) : Formula =
    match formula with
    | Implies (p, q) -> Or (Not (eliminateImplies p), eliminateImplies q)
    | And (p, q)     -> And (eliminateImplies p, eliminateImplies q)
    | Or (p, q)      -> Or  (eliminateImplies p, eliminateImplies q)
    | Not p          -> Not (eliminateImplies p)
    | f              -> f

let rec pushNot (formula: Formula) : Formula =
    match formula with
    | Not (Not p)      -> pushNot p
    | Not (And (p, q)) -> Or  (pushNot (Not p), pushNot (Not q))
    | Not (Or (p, q))  -> And (pushNot (Not p), pushNot (Not q))
    | And (p, q)       -> And (pushNot p, pushNot q)
    | Or  (p, q)       -> Or  (pushNot p, pushNot q)
    | Not p            -> Not p
    | f                -> f


let rec distributeOr (formula: Formula) : Formula =
    match formula with
    | Or (p, And (q, r)) -> And (distributeOr (Or (p, q)), distributeOr (Or (p, r)))
    | Or (And (q, r), p) -> And (distributeOr (Or (q, p)), distributeOr (Or (r, p)))
    | And (p, q)         -> And (distributeOr p, distributeOr q)
    | Or (p, q)          ->
        let p' = distributeOr p
        let q' = distributeOr q
        match p', q' with
        | _, And _ -> distributeOr (Or (p', q'))
        | And _, _ -> distributeOr (Or (p', q'))
        | _        -> Or (p', q')
    | f -> f

// Helpers for unifying literals into clauses
let rec collectLiterals (formula: Formula) : Clause =
    match formula with
    | Or (p, q)    -> Set.union (collectLiterals p) (collectLiterals q)
    | Atom s       -> Set.singleton (Pos s)
    | Not (Atom s) -> Set.singleton (Neg s)
    | _ -> failwith "Not a literal or clause"

let rec formulaToCNF (formula: Formula) : CNF =
    match formula with
    | And (p, q)    -> Set.union (formulaToCNF p) (formulaToCNF q)
    | Or (p, q)     -> Set.singleton (collectLiterals (Or (p, q)))
    | Atom s        -> Set.singleton (Set.singleton (Pos s))
    | Not (Atom s)  -> Set.singleton (Set.singleton (Neg s))
    | Tautology     -> Set.empty
    | Contradiction -> Set.singleton Set.empty
    | _             -> failwith "Not in CNF"

// Main conversion function
let convertToCNF (formula: Formula) : CNF =
    formula
    |> eliminateBiImplies
    |> eliminateImplies
    |> pushNot
    |> distributeOr
    |> formulaToCNF