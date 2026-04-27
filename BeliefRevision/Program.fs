open Logic
open CNFConversion
open Resolution

[<EntryPoint>]
let main _ =
    // KB: p. Query: q. Should be false — p does not entail q
    let kb2 = convertToCNF (Atom "p")
    let query2 = Atom "q"
    printfn "Entails q from p only: %b" (entails kb2 query2)
    0