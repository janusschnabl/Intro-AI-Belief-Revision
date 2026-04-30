open Logic
open CNFConversion
open Resolution
open BeliefBase
open AGMtests

[<EntryPoint>]
let main _ =
    let bb =
        []
        |> expand (Atom "p") 1
        |> expand (Implies (Atom "p", Atom "q")) 2
    
    printfn "Initial belief base:"
    printBeliefBase bb

    printfn "\nEntails q: %b" (entails bb (Atom "q"))

    let bb' = revise bb (Not (Atom "q")) 0

    printfn "\nAfter revision with ¬q:"
    printBeliefBase bb'

    printfn "\nEntails q: %b" (entails bb' (Atom "q"))


    let p = Atom "p"
    let phi = Atom "q"

    let B =
        []
        |> expand p 5
        |> expand (Implies(p, phi)) 10

    printTestResult (testInclussionPostulate B phi)

    0