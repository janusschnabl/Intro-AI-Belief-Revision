module AGMtests

open Logic
open CNFConversion
open Resolution
open BeliefBase

type Result =
    | PASS
    | FAIL
    | INDECISIVE

type TestResult = {
    result: Result
    name: string
}

let asResult b =
    if b then PASS else FAIL

let SuccesPostulateTest B phi =
    if not (entails [] phi) then
        let contracted = contract B phi
        {
            result = asResult (entails contracted phi)
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
let printTestResult testResult =
    let status =
        match testResult.result with
        | PASS -> "PASS"
        | FAIL -> "FAIL"
        | INDECISIVE -> "INDECISIVE"

    printfn "[%s] %s" status testResult.name

