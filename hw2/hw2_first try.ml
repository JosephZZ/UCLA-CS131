#use "dump.ml"

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

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


let rec print_list thelist = 
	match thelist with
	|[] -> Printf.printf "\n"
	|x::xs -> let () = Printf.printf "%s " (x) in
				print_list xs

let allPrefix l =
    let rec allPrefixHelper acc tillNow lst =
        match lst with 
        | [] ->  acc 
        | x::xs ->  let curPrefix = tillNow@[x] in 
                    allPrefixHelper (acc@[curPrefix]) curPrefix xs
    in
    allPrefixHelper [[]] [] l 


let rec suffix prefix oringinal =
	match prefix,oringinal with
	|[],ys -> ys
	|x::xs, y::ys -> if x = y
					 then suffix xs ys
					 else []

let derive (start,prod) frag =
	let rec derive_rules prod symbol rules tillNow frag=
		match rules with
		| [] -> ([],[],[])
		| first::rest -> 
			match (derive_rhs prod first (tillNow@[(symbol, first)]) frag) with
				|[],[],_ -> derive_rules prod symbol rest tillNow frag
				|der,frag_left,_ -> (der,frag_left,rest)

	and derive_rhs prod cpnts tillNow frag=
		match cpnts with
		| [] -> (tillNow, frag,[])
		| (N symbol)::rest -> (
							let rec rhs_helper prod symbol rules tillNow frag = 
								match (derive_rules prod symbol rules tillNow frag) with
										|[],[],[] -> [],[],[]			
										|der,frag_left, untried_rules -> 
											let thisWay = (derive_rhs prod rest der frag_left) in
												let () = dump thisWay in 
												if thisWay = ([],[],[])
												then rhs_helper prod symbol untried_rules tillNow frag
												else thisWay
							in
							rhs_helper prod symbol (prod symbol) tillNow frag)
		| (T symbol)::rest -> match frag with
										|[]    -> ([],[],[])
										|x::xs -> 
												  if x = symbol
												  then (
												  			derive_rhs prod rest tillNow xs)
												  else ([],[],[])
	in
	
	derive_rules prod start (prod start) [] frag	


(*
	let parse_prefix (start, prod) accept frag =
	let rec find_match (start, prod) accept tillNow lst =
		match lst with
		| []    -> None
		| x::suffix -> let curPrefix = (tillNow@[x]) in
						match (derive (start,prod) curPrefix) with
						|[],[]  -> find_match (start,prod) accept curPrefix suffix
						|der,_ -> let acceptReturn = accept der suffix in
									if acceptReturn = None
									then find_match (start,prod) accept curPrefix suffix
									else acceptReturn
	in
	find_match (start, prod) accept [] frag

	let () = List.iter (Printf.printf "current frag1 %s \n") frag in
*)

let parse_prefix (start, prod) accept frag =
	let rec find_match (start, prod) accept prefix_lst frag =
		match prefix_lst with
		| []    -> None
		| x::xs -> 
						match (derive (start,prod) x) with
						|[],[],_  -> find_match (start,prod) accept xs frag
						|der,frag_left,_ -> match frag_left with
											|[] ->(let acceptReturn = accept der (suffix x frag) in
													if acceptReturn = None
													then find_match (start,prod) accept xs frag
													else acceptReturn)
											|y::ys -> find_match (start,prod) accept xs frag
								 
	in
	let prefix_lst = List.rev (allPrefix frag)
	in
	find_match (start, prod) accept prefix_lst frag

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://www.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.
   Note that this grammar is not the same as Homework 1; it is
   instead the same as the grammar under "Theoretical background"
   above.  *)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)