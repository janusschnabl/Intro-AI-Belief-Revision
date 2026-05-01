module AGMRevisionTests

open Logic
open BeliefBase
open TestResultHelper

let SuccesPostulateTest B phi =
    {
        result = asResult (entails (revise B phi 5) phi)
        name = "Succes"
    }

let AGMRevisionTests =
    printfn "Running revision postulate tests!"

    let p = Atom "p"
    let q = Atom "q"

    let B1 =
        []
        |> expand p 5
        |> expand (Implies(p, q)) 10

    let B2 =
        []
        |> expand p 5

    let phiEquivalent1 = Implies(p, q)
    let phiEquivalent2 = Or(Not p, q)

    let tests =
        [
            SuccesPostulateTest 
        ]

    tests |> List.iter (fun t -> printfn "%s" (toString t))