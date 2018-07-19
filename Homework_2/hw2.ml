type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec prod_func_list rules non_term =
	match rules with
	| [] -> []
	| h::t -> let rest = prod_func_list t non_term in 
	if non_term = (fst h) then (snd h)::rest else rest

(* gram: A pair consisting of a start symbol and a list of rules
 * non_term: The nonterminal symbol
 * Returns gram's alternative list for that nonterminal *)
let prod_func gram non_term = 
	prod_func_list (snd gram) non_term

(* gram1: A pair consisting of a start symbol and a list of rules
 * Returns a pair consisting of a start symbol and a production function *)
let convert_grammar gram1 = 
	(fst gram1, prod_func gram1)


(* Currying is too confusing, just pass all the arguments lmao *)

(* Arguments: 
   sym: The nonterminal we are currently trying to match to a sequence of terminals in frag
   rules: Alternative list i.e. all the right hand sides that sym can be expanded to 
   get_rules: Function to get the alternative list for a given nonterminal
   acceptor: Function that returns an option showing if the fragment is acceptable or not
   deriv: List of rules that explains how we got here, "history" of the parser
   frag: Fragment we are matching to *)
let rec match_nonterm sym rules get_rules acceptor deriv frag = 
    match rules with
    (* If there are no more rules that can match the nonterminal, then we can't match fragment; return None *)
    | [] -> None
    | current_rule::other_rules -> 
        (* If acceptor likes the suffix where we apply rule h, then we can just return it *)
        (* To get the prefix, try to match the current non_terminal to a terminal symbol in the fragment *)
        (* Append the current rule that we're trying to the derivation 
         * get_rules = get_rules, production function stays the same 
         * current_rule = current_rule, try to match a sequence of terminals to the current rule 
         * acceptor = acceptor 
         * deriv = deriv@(sym, current_rule), append the symbol and the rule we are trying to the derivation in case it succeeds
         * frag = frag *)
        let prefix = match_term get_rules current_rule acceptor (deriv@[sym, current_rule]) frag in 
        match prefix with
        (* Acceptor didn't like this rule being applied, try the next rule in rules *)
        | None -> match_nonterm sym other_rules get_rules acceptor deriv frag
        (* Acceptor liked this rule, so we're done! Equivalent to returning prefix *)
        | _ -> prefix
(* Arguments: 
 * get_rules: Production function 
 * current_rule: The rule that we are currently trying to match a string of terminals to
 * acceptor: Function that returns an option showing if the fragment is acceptable or not
 * deriv: List of rules that explains how we got here, "history" of the parser
 * frag: Fragment we are matching to 
 *)
and match_term get_rules current_rule acceptor deriv frag =
    match current_rule with
    (* If there are no more symbols in this rule, then we have to hope that the acceptor likes our fragment *)
    | [] -> (acceptor deriv frag)
    (* If you switch the order of N and T it breaks for some reason lmao *)
    (* If the next symbol in the rule is nonterminal, then ask match_nonterm to match it with:
     * sym = non_term, the new nonterminal to match
     * rules = (get_rules non_term), list of rules for the new nonterminal 
     * get_rules = get_rules, production function should stay the same 
     * acceptor = (match_term get_rules rest_of_rule acceptor), acceptor that has the history builtin 
     * deriv = deriv
     * frag = frag *)
    | (N non_term)::rest_of_rule -> (match_nonterm non_term (get_rules non_term) get_rules (match_term get_rules rest_of_rule acceptor) deriv frag)
    (* If the next symbol in the rule is terminal, then check to see if the beginning of frag is that terminal *)
    | (T term)::rest_of_rule -> match frag with 
                                (* frag is empty, this rule doesn't work, return None to show not accepted *)
                                | [] -> None
                                (* If first matches the terminal required by the rule, check if the rest_of_frag matches rest_of_rule with:
                                 * get_rules = get_rules, production function stays the same
                                 * current_rule = rest_of_rule, since we just matched the first symbol of the rule 
                                 * acceptor = acceptor
                                 * deriv = deriv
                                 * frag = rest_of_frag, just committed the first symbol of frag to being part of rule *)
                                | first::rest_of_frag -> if term = first then match_term get_rules rest_of_rule acceptor deriv rest_of_frag else None

    (* Attempt to match from the start symbol: 
     * sym = (fst gram), the start symbol 
     * rules = (snd gram) (fst gram)
     * get_rules = (snd gram)
     * acceptor = acceptor
     * deriv = [], no initial progress 
     * frag = frag, entire fragment to match *)
let parse_prefix gram acceptor frag = 
	match_nonterm (fst gram) ((snd gram) (fst gram)) (snd gram) acceptor [] frag
