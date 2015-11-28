type typ =
    | Tany
    | TanyVal
    | Tboolean
    | Tint
    | Tunit
    | TanyRef
    | Tstring
    | Tnull
    | Tnothing
    | Tclasse of typ list
    | Tmethode of typ list * typ
