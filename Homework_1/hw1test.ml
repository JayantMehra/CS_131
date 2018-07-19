(* Define our grammar stuff up here to make test case output easier to read *)
type my_test_nonterminals = | Sentence | NP | VP | Article | Adjective | Adverb | Noun | Verb

let my_good_rules = 
	[ Sentence, [N NP; N VP;T "."];
	  NP, [N Adjective; N Noun];
	  Adjective, [N Article; N Adjective];
	  Adjective, [T "quiet"];
	  Adjective, [T "loud"];
	  Article, [T "A"];
	  Article, [T "The"];
	  VP, [N Verb; N Adverb];
	  Adverb, [T "quietly"];
	  Adverb, [T "loudly"];	
	  Noun, [T "mouse"];
	  Noun, [T "elephant"];
	  Verb, [T "walked"];
	  Verb, [T "ran"]
	]

let my_bad_rules = 
	[ Sentence, [N NP; N VP;T "."];
	  Adjective, [N Article; N Adjective];
	  Adjective, [T "quiet"];
	  Adjective, [T "loud"];
	  Article, [T "A"];
	  Article, [T "The"];
	  VP, [N Verb; N Adverb];
	  Adverb, [T "quietly"];
	  Adverb, [T "loudly"];	
	  Noun, [T "mouse"];
	  Noun, [T "elephant"];
	  Verb, [T "walked"];
	  Verb, [T "ran"]
	]

let my_terrible_rules = 
	[ Sentence, [N NP; N VP;T "."];
	  NP, [N Adjective; N Noun];
	  Adjective, [N Article; N Adjective];
	  VP, [N Verb; N Adverb];
	]

let my_good_grammar = (Sentence, my_good_rules)
let my_bad_grammar = (Sentence, my_bad_rules)
let my_terrible_grammar = (Sentence, my_terrible_rules)

(* subset test cases *)
let my_subset_test0 = subset [1;2] [1;2;3]
let my_subset_test1 = not (subset [1;2] [1;4])
let my_subset_test2 = subset [1] [1;2]
let my_subset_test3 = subset [] [1;4]
let my_subset_test4 = not (subset [1;2] [])
let my_subset_test5 = subset [] []

(* equal_subsets test cases *)
let my_equal_sets_test0 = equal_sets [1;2] [1;2]
let my_equal_sets_test1 = equal_sets [1;2] [2;1]
let my_equal_sets_test2 = not (equal_sets [1;2] [3;4])
let my_equal_sets_test3 = equal_sets [1] [1]
let my_equal_sets_test4 = not (equal_sets [1;2] [])
let my_equal_sets_test5 = not (equal_sets [] [1;2])
let my_equal_sets_test6 = equal_sets [] []

(* set_union test cases *)
let my_set_union_test0 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]
let my_set_union_test1 = equal_sets (set_union [1;2] [2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1;2] [1;2]) [1;2]
let my_set_union_test3 = equal_sets (set_union [1] [2]) [1;2]
let my_set_union_test4 = equal_sets (set_union [] [1;2]) [1;2]
let my_set_union_test5 = equal_sets (set_union [1;2] []) [1;2]
let my_set_union_test6 = equal_sets (set_union [] []) []
let my_set_union_test7 = equal_sets (set_union [1;2;3;4] [1;2]) [1;2;3;4]
let my_set_union_test8 = equal_sets (set_union [1;2;3;4] [1;2;5]) [1;2;3;4;5]

(* set_intersection test cases *)
let my_set_intersection_test0 = equal_sets (set_intersection [1;2] [2;3]) [2]
let my_set_intersection_test1 = equal_sets (set_intersection [1;2] [3;4]) []
let my_set_intersection_test3 = equal_sets (set_intersection [1;2] [1;2]) [1;2]
let my_set_intersection_test4 = equal_sets (set_intersection [1] [1]) [1]
let my_set_intersection_test5 = equal_sets (set_intersection [] [1;2]) []
let my_set_intersection_test6 = equal_sets (set_intersection [1;2] []) []
let my_set_intersection_test7 = equal_sets (set_intersection [] []) []
let my_set_intersection_test8 = equal_sets (set_intersection [1;1;1;1] [1]) [1]

(* set_diff test cases *)
let my_set_diff_test0 = equal_sets (set_diff [1;2] [3;4]) [1;2]
let my_set_diff_test1 = equal_sets (set_diff [1;2] [2;3]) [1]
let my_set_diff_test2 = equal_sets (set_diff [1;2] [1;2]) []
let my_set_diff_test3 = equal_sets (set_diff [1] [2]) [1]
let my_set_diff_test5 = equal_sets (set_diff [] [1;2]) []
let my_set_diff_test6 = equal_sets (set_diff [1;2] []) [1;2]
let my_set_diff_test7 = equal_sets (set_diff [] []) []

(* computed_fixed_point test cases *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 4) 1000 = 0

(* computed_periodic_point test cases *)
let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> -x) 2 1000 = 1000

(* while_away test cases *)
let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9]
let my_while_away_test1 = equal_sets (while_away ((+) 1) ((>) 5) 1) [1;2;3;4]
let my_while_away_test2 = equal_sets (while_away ((+) 3) ((>) 10) 11) []
	
(* rle_decode test cases *)
let my_rle_decode_test0 = rle_decode [(3, "w"); (2, "o"); (5, "w")] = ["w";"w";"w";"o";"o";"w";"w";"w";"w";"w"]
let my_rle_decode_test1 = rle_decode [(3, "w")] = ["w";"w";"w"]
let my_rle_decode_test2 = rle_decode [(3, "w"); (1, "o")] = ["w";"w";"w";"o"]
let my_rle_decode_test3 = rle_decode [] = []
let my_rle_decode_test4 = rle_decode [(3, "w"); (0, "o")] = ["w";"w";"w"]
let my_rle_decode_test5 = rle_decode [(0, "o"); (3, "w")] = ["w";"w";"w"]

(* filter_blind_alleys test cases *)
let my_filter_blind_alleys_test0 = filter_blind_alleys my_good_grammar = my_good_grammar
let my_filter_blind_alleys_test1 = filter_blind_alleys my_bad_grammar = 
	(Sentence, [Adjective, [N Article; N Adjective];
	  Adjective, [T "quiet"];
	  Adjective, [T "loud"];
	  Article, [T "A"];
	  Article, [T "The"];
	  VP, [N Verb; N Adverb];
	  Adverb, [T "quietly"];
	  Adverb, [T "loudly"];	
	  Noun, [T "mouse"];
	  Noun, [T "elephant"];
	  Verb, [T "walked"];
	  Verb, [T "ran"]
	])
let my_filter_blind_alleys_test2 = filter_blind_alleys my_terrible_grammar = (Sentence, [])
