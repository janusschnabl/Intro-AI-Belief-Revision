module TestResultHelper

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

let toString testResult =
    let status =
        match testResult.result with
        | PASS -> "PASS"
        | FAIL -> "FAIL"
        | INDECISIVE -> "INDECISIVE"

    sprintf "[%s] %s" status testResult.name