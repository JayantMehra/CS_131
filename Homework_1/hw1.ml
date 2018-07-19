(* Returns true if a is a subset of b *)
let rec subset a b = 
    match a with
    | [] -> true
    | hd::tl -> 
      if List.mem hd b then subset tl b else false 

(* Returns true if a and b have the same contents *)
let equal_sets a b = 
    (subset a b) && (subset b a)

(* Returns a list containing the union of a and b  *)
let rec set_union a b =
    match a with
    | [] -> b
    | hd::tl ->
      let rest = set_union tl b in
      if List.mem hd b then rest else hd::rest

let rec set_intersection a b = 
    match a with
    | [] -> []
    | hd::tl ->
      let rest = set_intersection tl b in
      if List.mem hd b then hd::rest else rest

let rec set_diff a b =
    match a with 
    | [] -> []
    | hd::tl ->
      let rest = set_diff tl b in
      if List.mem hd b then rest else hd::rest

let rec computed_fixed_point eq f x = 
    if eq (f x) x then x else computed_fixed_point eq f (f x)

(* Computes the result of calling f on x p times *)
let rec compute_function f p x =
    if p = 0 then x else compute_function f (p-1) (f x)

let rec computed_periodic_point eq f p x =
    if p = 0 then x 
    else (if eq (compute_function f p x) x then x 
          else computed_periodic_point eq f p (f x))

let rec while_away s p x = 
    if p x then x::(while_away s p (s x)) else []

(* Repeats string s n times *)
let rec repeat_string s n =
    if n = 0 then [] else 
    let rest = repeat_string s (n-1) in s::rest

let rec rle_decode lp =
    match lp with
    | [] -> []
    | hd::tl ->
      let (count, str) = hd and 
      rest = rle_decode tl in (repeat_string str count)@rest

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Returns true if sym is in left hand side of any rule in rules 
For some reason using List.mem instead of this breaks the code *)
let rec contains sym rules = 
    match rules with 
    | [] -> false
    | hd::tl -> 
    if hd = sym then true else contains sym tl

(* Returns true if sym is terminal or a known good non-terminal *)
let rec is_good sym good_rules =
    match sym with
    | T _ -> true
    | N non -> (contains non good_rules)

(* Returns true if the right hand side consists only of good symbols *)
let rec has_only_good rhs good_rules =
    match rhs with
    | [] -> true
    | hd::tl -> 
    if is_good hd good_rules then has_only_good tl good_rules else false

let rec get_good_symbols rules good_symbols =
    match rules with 
    | [] -> good_symbols
    | hd::tl ->
    let rest = get_good_symbols tl good_symbols in
    if has_only_good (snd hd) good_symbols && not (contains (fst hd) good_symbols) then (fst hd)::rest else rest

let rec get_all_good_symbols rules initial_good = 
    if equal_sets (get_good_symbols rules initial_good) initial_good then initial_good else get_all_good_symbols rules (get_good_symbols rules initial_good)

let rec get_good_rules rules good_sym =
    match rules with
    | [] -> []
    | hd::tl ->
    let rest = get_good_rules tl good_sym in
    if has_only_good (snd hd) good_sym then hd::rest else rest

let filter_blind_alleys g =
    let rules = snd g in
    (fst g), get_good_rules rules (get_all_good_symbols rules [])
