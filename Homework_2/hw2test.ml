type math_nonterminals = 
    | Expr | Num | Var | Binop

let accept_all derivation string = Some(derivation, string)
let accept_empty derivation = function 
    | [] -> Some(derivation, [])
    | _ -> None

let math_grammar = 
    (Expr, function 
        | Expr -> [[T "("; N Expr; T ")"];
                    [N Var; N Binop; N Var];
                    [N Num; N Binop; N Num];
                    [N Num; N Binop; N Expr]]
        | Num -> [[T "1"]; [T "2"]]
        | Var -> [[T "x"]; [T "y"]]
        | Binop -> [[T "+"]; [T "-"]]
    )

let test_1 = parse_prefix math_grammar accept_all  ["2"; "+"; "("; "x" ; "+"; "y"; ")"] = Some
   ([(Expr, [N Num; N Binop; N Expr]); (Num, [T "2"]); (Binop, [T "+"]);
     (Expr, [T "("; N Expr; T ")"]); (Expr, [N Var; N Binop; N Var]);
     (Var, [T "x"]); (Binop, [T "+"]); (Var, [T "y"])],
    [])
(* This test case has some ambiguity, make sure it parses as 2 + (1 + (1 + 2)) *)
let test_2 = parse_prefix math_grammar accept_empty ["2";"+";"1";"+";"1";"+";"2"] = Some
   ([(Expr, [N Num; N Binop; N Expr]); (Num, [T "2"]); (Binop, [T "+"]);
     (Expr, [N Num; N Binop; N Expr]); (Num, [T "1"]); (Binop, [T "+"]);
     (Expr, [N Num; N Binop; N Num]); (Num, [T "1"]); (Binop, [T "+"]);
     (Num, [T "2"])],
    [])
