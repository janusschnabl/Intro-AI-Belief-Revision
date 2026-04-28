module BeliefBase

open Logic
open CNFConversion

type Belief = {
    formula: Formula
    priority: int
}

type BeliefBase = Belief list

// Helper: Convert BB to CNF
let toCNF (bb: BeliefBase) : CNF =
    bb
    |> List.map (fun b -> convertToCNF b.formula)
    |> Set.unionMany

// Entailment
let entails (bb: BeliefBase) (f: Formula) : bool =
    let cnf = toCNF bb
    Resolution.entails cnf f

// Expansion
let expand (f: Formula) (priority: int) (bb: BeliefBase) : BeliefBase =
    { formula = f; priority = priority } :: bb

// Contraction
let contract (bb: BeliefBase) (f: Formula) : BeliefBase =
    let sorted = List.sortByDescending (fun b -> b.priority) bb

    let rec remove beliefs =
        if not (entails beliefs f) then
            beliefs
        else
            match beliefs with
            | [] -> []
            | _ :: rest -> remove rest
        
    remove sorted

// Revision
let revise (bb: BeliefBase) (f: Formula) (priority: int) : BeliefBase =
    let contracted = contract bb (Not f)
    expand f priority contracted

let literalToString lit =
    match lit with
    | Pos s -> s
    | Neg s -> "¬" + s

let clauseToString (c: Clause) =
    c
    |> Set.map literalToString
    |> String.concat " ∨ "

let cnfToString (cnf: CNF) =
    cnf
    |> Set.map (fun c -> "(" + clauseToString c + ")")
    |> String.concat " ∧ "

let beliefToString (b: Belief) =
    let cnf = convertToCNF b.formula
    sprintf "[p=%d] %s" b.priority (cnfToString cnf)

let printBeliefBase (bb: BeliefBase) =
    bb
    |> List.map beliefToString
    |> String.concat "\n"
    |> printfn "%s"