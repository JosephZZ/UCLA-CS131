  (*
Today:
Going over more stuff that could help with homework 2

Warm up: simple dictionary
represent dictionary as functions
reminder: List.map, List.filter, List.rev
List.fold_left, List.fold_right
tree
allPrefix
allSufix
encode
*)



(*
Warm up:
Our first implementation of a dictionary is as an association
list, i.e. a list of pairs. Implement empty1, put1, and get1 for
association lists (we use the suffix 1 to distinguish from
other implementations below). 
As an example of how this representation of dictionaries works,
the dictionary that maps "hello" to 5 and has no other entries
is represented as [("hello",5)]. To get the effect of replacing
old entries for a key, put1 should simply add new entries to
the front of the list, and accordingly get1 should return the 
leftmost value whose associated key matches the given key.
*)


let empty1 ()  = []

let put1 key value dict = 
    (key,value)::dict 

let rec get1 key dict =
    match dict with
    | [] -> None 
    | (k,value)::tails -> if k = key then
                            Some value
                          else 
                            get1 key tails 

(*
Conceptually a dictionary is just a function from keys to 
values. Since OCaml has first-class functions, we can choose to
represent dictionaries as actual functions. We define the
following type:

type ('a,'b) dict3 = ('a -> 'b option)

Here dict3 is a type synonym: it is just a shorthand for the 
givenfunction type rather than a new type. As an example of how
this representation of dictionaries works, the following
dictionary maps "hello" to 5 and has no other entries:

(function s ->
  match s with
    "hello" -> Some 5
  | _ -> None)


One advantage of this representation over the  dictionary 
implementations above is that we can represent infinite-size
dictionaries.For instance, the following dictionary maps all 
strings to their length (using the String.length function):

(function s -> Some(String.length s))
Implement empty3, put3, and get3 for dict3. 
It's fine if the types that OCaml infers for these functions
use ('a -> 'b option) in place of ('a,'b) dict3, since they
are synonyms for one another.
*)

type ('a,'b) dict3 = ('a -> 'b option )

let empty3() : ('a,'b) dict3 =  function x -> None 

let put3 k v (d:('a,'b) dict3) : ('a,'b)  dict3 = 
    function x ->  if x = k then 
                     Some v
                   else 
                     d x 

let get3 k d =
    d k 


(*
 List.rev, List.map, List.flatten, List.filter reminder
*)

List.rev [1;2;3];; (*returns [3;2;1]*)

List.map  ( fun x -> x+1 ) [1;2;3];;



List.fold_right (+)  [1;2;3] 0
(*
List.fold_left
List.fold_left  f  b [x1;x2;...xn] evaluate to
f( f ( f ( ... f(b x1 ) x2 )))
the 'left' means we traverse the list from left to right 


List.fold_right 
traverses the list from right to left 
evaluates to 
f x1 ( f x2 ( f ... (f xn b)))

 *)

(*returns max integer of list, assume list is non-negative
 * and not empty *)
let max_int l = 
    List.fold_left (fun x acc -> if x > acc then x else acc) 0 l 


(*
split [(1,"a");(3,"b");(5,"c")] -> ([1;3;5], ["a";"b";"c"] ) 
*)

let split l = 
    List.fold_right  (fun (x,y) (xs,ys) -> (x::xs, y::ys ))  l ([],[]) 



(*List.map using List.fold_right*)
let map f l  = 
    List.fold_right (fun e acc -> (f e)::acc ) l []



type tree = Node of int * tree list 

let funtree = Node(1,
                    [ Node(2, []);
                      Node(3,
                        [ Node(4,[]); Node(5,[]);Node(6,[]) ] 
                        ) ] )

let rec reflectTree (Node(n, lst ))  =
    Node(n, List.rev ( List.map reflectTree lst ))

(*
 Prefix: [1;2;3]
 [1],[1;2],[1;2;3],[]
 Suffix: [], [3],[2;3],[1;2;3]

 
 *)

let rec prefixOf l1 l2 =
    match l1 with
    | [] -> true
    | x::xs -> match l2 with
                | [] -> false
                | y::ys -> if x = y then prefixOf xs ys else false 


let allPrefix l =
    let rec allPrefixHelper acc tillNow lst =
        match lst with 
        | [] ->  acc 
        | x::xs ->  let curPrefix = tillNow@[x] in 
                    allPrefixHelper (acc@[curPrefix]) curPrefix xs
    in
    allPrefixHelper [[]] [] l 


let allSufix l = 
   List.map List.rev (  allPrefix (List.rev l ) )
    

