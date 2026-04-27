module Logic

type Formula =
    | Atom of string              
    | Not of Formula
    | And of Formula * Formula
    | Or  of Formula * Formula
    | Implies    of Formula * Formula
    | BiImplies  of Formula * Formula
    | Tautology                  
    | Contradiction       