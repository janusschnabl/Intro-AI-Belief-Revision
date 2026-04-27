open Logic
open CNFConversion

[<EntryPoint>]
let main _ =
    // Test: ¬(p → q) ∨ r  should give {{p, r}, {¬q, r}}
    let f = Or (Not (Implies (Atom "p", Atom "q")), Atom "r")
    let cnf = convertToCNF f
    printfn "%A" cnf
    0