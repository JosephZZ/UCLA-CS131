#use "hw2.ml"

type conv_nonterminals =
  | Conversation | Sentence | Filler | Subject | Verb | Object | Swear

let conversation_grammar =
	(Conversation, 
		(function
			| Conversation ->
				[[N Sentence; N Filler; N Conversation];
				 [N Sentence]]
			| Sentence ->
				[[N Subject; N Verb; N Object];
				[N Subject; N Verb];
				[N Swear]]
			| Filler ->
				[[T"and"];
				 [T"."]]
			| Subject ->
				[[T"Foo"];
				 [T"Bar"]]
			| Verb ->
				[[T"is"];
				 [T"likes"]]
			| Object ->
				[[T"fool"];
				 [T"Emacs"]]
			| Swear ->
				[[T"Nonsense"]])
	)

let test_1 = 
	((parse_prefix conversation_grammar accept_all ["Foo";"is";"fool";"and";"Bar";"likes";"Emacs";".";"Nonsense"])
		= Some
			 ([(Conversation, [N Sentence; N Filler; N Conversation]);
			   (Sentence, [N Subject; N Verb; N Object]); (Subject, [T "Foo"]);
			   (Verb, [T "is"]); (Object, [T "fool"]); (Filler, [T "and"]);
			   (Conversation, [N Sentence; N Filler; N Conversation]);
			   (Sentence, [N Subject; N Verb; N Object]); (Subject, [T "Bar"]);
			   (Verb, [T "likes"]); (Object, [T "Emacs"]); (Filler, [T "."]);
			   (Conversation, [N Sentence]); (Sentence, [N Swear]);
			   (Swear, [T "Nonsense"])],
			  [])
		)

let accept_filler derivation suffix = 
	match suffix with
	|x::xs -> match x with
				|"and"
				|"." -> Some (derivation, suffix)
				|_ -> None

let test_2 = ((parse_prefix conversation_grammar accept_filler ["Foo";"likes";"fool";"and";"Nonsense";"and";"Bar";"is";"Emacs";".";"Nonsense";"."])
				=
				Some
				 ([(Conversation, [N Sentence; N Filler; N Conversation]);
				   (Sentence, [N Subject; N Verb; N Object]); (Subject, [T "Foo"]);
				   (Verb, [T "likes"]); (Object, [T "fool"]); (Filler, [T "and"]);
				   (Conversation, [N Sentence; N Filler; N Conversation]);
				   (Sentence, [N Swear]); (Swear, [T "Nonsense"]); (Filler, [T "and"]);
				   (Conversation, [N Sentence; N Filler; N Conversation]);
				   (Sentence, [N Subject; N Verb; N Object]); (Subject, [T "Bar"]);
				   (Verb, [T "is"]); (Object, [T "Emacs"]); (Filler, [T "."]);
				   (Conversation, [N Sentence]); (Sentence, [N Swear]);
				   (Swear, [T "Nonsense"])],
				  ["."])
				)

