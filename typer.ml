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
    | Tvar of tvar
    | Tclasse of typ list
    | Tmethode of typ list * typ

and tvar = {
    id          : int ;
    mutable def : typ option }

module V = struct
    type t  = tvar
    let compare v1 v2   = Pervasives.compare v1.id v2.id
    let equal   v1 v2   = v1.id = v2.id
    let create          =   let r = ref 0
                            in  fun () -> incr r;
                                {id     = !r ;
                                def     = None }
