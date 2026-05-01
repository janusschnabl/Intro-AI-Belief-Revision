module AGMContractionTests
open Logic
open BeliefBase
open TestResultHelper

let SuccesPostulateTest B phi =
    if not (entails [] phi) then
        let contracted = contract B phi
        {
            result = asResult (not (entails contracted phi))
            name = "Succes"
        }
    else 
        {
            result = INDECISIVE
            name = "Success (phi is tautology)"
        }

// Helper for Inclussion postulate test
let isSubsetOf small large =
    List.forall(fun x -> List.contains x large) small

let InclussionPostulateTest B phi =
    let contracted = contract B phi
    {
        result = asResult (isSubsetOf contracted B)
        name = "Inclussion"
    }

// Helper for Vacuity postulate test
let equals list1 list2 =
    isSubsetOf list1 list2 && isSubsetOf list2 list1

let VacuityPostulateTest B phi =
    if not (entails B phi) then
        let contracted = contract B phi
        {
            result = asResult (equals B contracted)
            name = "Vacuity"
        }
    else
        {
            result = INDECISIVE
            name = "Vacuity (phi is already in Cn(B))"
        }

let ExtensionalityPostulateTest B phi psi =
    if entails [] (BiImplies(phi, psi)) then
        let contracted1 = contract B psi
        let contracted2 = contract B phi
        {
            result = asResult (equals contracted1 contracted2)
            name = "ExtensionalityPostulateTest"
        }
    else 
        {
            result = INDECISIVE
            name = "ExtensionalityPostulateTest (The bi-implication of phi and psi are not a tautology)"
        }

let RunAllAGMContractionTests =
    printfn "Running contraction postulate tests!"
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
            SuccesPostulateTest B1 q

            InclussionPostulateTest B1 q

            VacuityPostulateTest B2 q

            ExtensionalityPostulateTest B1 phiEquivalent1 phiEquivalent2
        ]

    tests |> List.iter (fun t -> printfn "%s" (toString t))


