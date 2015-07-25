let rec filter_rules rule_list symbol_name =
	match rule_list with
	|[] -> []
	|(symbol,content)::tails -> if symbol = symbol_name
								then [content]@(filter_rules tails symbol_name)
								else filter_rules tails symbol_name

let convert_grammar old_grammar =
	match old_grammar with
	|start,rule_list ->
		let prod = filter_rules rule_list in
		(start, prod)

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let rec match_rules prod symbol rules accept deri frag=
	match rules with
	|[] -> None
	|first::rest -> match match_rhs prod first accept (deri@[symbol,first]) frag with
					|None -> match_rules prod symbol rest accept deri frag
					|result -> result

and match_rhs prod rhs accept deri frag =
	match rhs with
	|[] -> accept deri frag
	|sym::sym_rest-> match sym with
		|(N symbol_n) -> match_rules prod symbol_n (prod symbol_n) (match_rhs prod sym_rest accept) deri frag
		|(T symbol_t) -> match frag with
							|[] -> None
							|car::cdr -> if symbol_t = car
										 then match_rhs prod sym_rest accept deri cdr
										 else None
						
		

let parse_prefix (start, prod) accept frag =
	match_rules prod start (prod start) accept [] frag