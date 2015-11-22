type fichier = {
    classes     : classe list ;
    main        : classe_Main ; }
type classe = {
    name        : indent ;
    type_params : (param_type_classe list) option ;
    params      : (parametre list) option ; (*paramètres*)
    deriv       : typ * (expr list) ; (*héritage*)
    decls       : decl list ; }


