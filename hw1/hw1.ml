let rec subset a b =
        match a with
        | [] -> true
        | x::xs -> match b with
                    | [] -> false
                    | y::ys -> if x =y 
                    		   then subset xs b
                               else subset [x] ys && subset xs b


let rec cleanedSet a b =
        match b with
        | [] -> []
        | h::t -> if a=h then cleanedSet a t
                          else h::(cleanedSet a t)


let rec equal_sets a b =
        if a=b then true else
                match a,b with
                |[],[] -> true
                |[],y -> false
                |x,[] -> false
                |hx::tx,_ -> if subset [hx] b
                             then equal_sets (cleanedSet hx tx) (cleanedSet hx b)
                             else false

let rec set_union a b =
	cleanRepeated a@b


let rec set_intersection a b =
        match a,b with
        | [],_ -> []
        | x::xs, _ -> if subset [x] b
                      then x::set_intersection (cleanedSet x xs) (b)
                      else set_intersection xs b


let rec cleanRepeated a =
        match a with
        | [] -> []
        | x::xs -> x::cleanRepeated (cleanedSet x xs)


let rec set_diff a b =
        match a,b with
        | [], _ -> []
        | a,[] -> cleanRepeated a
        | x::xs, y -> if subset [x] y
                      then set_diff xs b
                      else x::set_diff (cleanedSet x xs) b

let rec computed_fixed_point eq f x =
	if 		eq (f x) x
	then    x
	else    computed_fixed_point eq f (f x)



let rec computed_periodic_point_helper f p x =
	match p with
	| 0 -> x
	| _ -> computed_periodic_point_helper f (p-1) (f x)


let rec  computed_periodic_point eq f p x =
	if   eq (computed_periodic_point_helper f p x) x
	then x
	else computed_periodic_point eq f p (f x)


(* ALL the following functions filter out blind alley rules for given grammar.
   The approach I take is 
	1, seperate the grammar rules into nonTerminal list and terminal list
	2, scan through all the nonTerminal rules to see if any of them can reach to the terminal with the current terminal list
	3, if found any in Step2, put them into the terminal list
	4, repeat from Step2 until there's no nonTerminal rules can be added
*)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal


let rec checkTerminal content =
	match content with
	| [] -> true
	| h::t -> match h with
				| T x -> checkTerminal t
				| N x -> false

let rec getTerminalExpressions rules =
	match rules with
	| [] -> []
	| h::t -> match h with
				| _, content -> if checkTerminal content
							    then h::(getTerminalExpressions t)
							    else getTerminalExpressions t

let rec getNonTerminalExpressions rules terminals =
	set_diff rules terminals

let rec goodExpr expr terminals =
	match terminals with
	| [] -> false
	| t::ts -> match t with
			| typeName, _ -> 
				  if typeName = expr
				  then true 
				  else goodExpr expr ts

(*check if it's good - meaning not blind alley - meaning it's able to reach terminal*)
let rec goodRule rule terminals =
	match rule with
	| (x, []) -> true
	| (x, expr::exprs) -> match expr with
						| T symbol -> goodRule (x, exprs) terminals
						| N symbol -> if goodExpr symbol terminals
					    				then goodRule (x,exprs) terminals
										else false

(* processRules does ONE sequestial scan over the non terminal rules and see if any of it is not blind alley, 
   if yes put it into the terminal list.
   Notice one round of scan cannot guarantee complete filter. Later we'll use computed_fixed_point to make sure
   they are scanned enough times to take all the non-blind-alley rules*)
let rec processRules grammar =
	match grammar with
	| terminals,nonTerminals ->
		let length = List.length nonTerminals in
			let rec processRulesInner terminals nonTerminals i =
				if i = length then (terminals, nonTerminals) else
					match nonTerminals with
					| head_rule::tail_rules -> 
						if goodRule head_rule terminals
						then processRulesInner ([head_rule]@terminals) (tail_rules@[head_rule]) (i+1)
						else processRulesInner terminals (tail_rules@[head_rule]) (i+1)
			in processRulesInner terminals nonTerminals 0

(* rules_equal is the 'eq' function we'll use in computed_fixed_point function*)
let rules_equal previous current =
	match previous,current with
	| (previous_ters,_),(current_ters,_) ->
		if equal_sets previous_ters current_ters
		then true
		else false

(* reorder the terminals in sequence with the grammar rules given at first*)
let rec ordered_rules rules terminals =
	match rules with
	| [] -> []
	| h::t -> if subset [h] terminals
			  then [h]@(ordered_rules t terminals)
			  else ordered_rules t terminals

(* The boss function.*)
let filter_blind_alleys grammar =
	match grammar with
	| start, rules ->
		let terminals = getTerminalExpressions rules in
			let nonTerminals = getNonTerminalExpressions rules terminals in
				match computed_fixed_point rules_equal processRules (terminals,nonTerminals) with
				|results,_ -> start, (ordered_rules rules results)







