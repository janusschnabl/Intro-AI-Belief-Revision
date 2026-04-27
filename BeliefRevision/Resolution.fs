module Resolution

open Logic
open CNFConversion

let resolve (clause1: Clause) (clause2: Clause) : Set<Clause> =
    let resolvents =
        [ for l1 in clause1 do
            for l2 in clause2 do
                match l1, l2 with
                | Pos p, Neg q when p = q ->
                    yield Set.union (Set.remove l1 clause1) (Set.remove l2 clause2)
                | Neg p, Pos q when p = q ->
                    yield Set.union (Set.remove l1 clause1) (Set.remove l2 clause2)
                | _ -> () ]
    Set.ofList resolvents



let rec resolution (clauses: CNF) : bool =
    let allPairs =
        [ for c1 in clauses do
            for c2 in clauses do
                if c1 <> c2 then
                    yield! resolve c1 c2 ]
    let newClauses = Set.ofList allPairs
    if Set.contains Set.empty newClauses then
        true  // empty clause derived — unsatisfiable
    else
        let combined = Set.union clauses newClauses
        if Set.isSubset combined clauses then
            false  // no new clauses — satisfiable
        else
            resolution combined  // keep going


let entails (kb: CNF) (formula: Formula) : bool =
    let negated = Not formula
    let negatedCNF = convertToCNF negated
    let combined = Set.union kb negatedCNF
    resolution combined