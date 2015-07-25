(*
TA Name: Tomer Weiss
Email:   tweiss@cs.ucla.edu

Section notes uploaded to CCLE

Office hours update: Monday,Tuesday 10-11AM, TA room, BH

User Defined Types:
- Type names should be small case
- Constructors should start with CAP
- Constructors are not always necessary
- A new type can be defined just to shorten the name
  -- E.g. type point = float * float
- Variant types MUST have constructors
  -- E.g. type intOrString = I of int | S of string
- Without a constructor, OCaml “infers” the type in terms of base types
- Constructors can be nullary (no arguments required)

Examples:
type sign = Positive | Negative | Zero

type shape = Rect of point * point		(* left-top and right-bottom corners *)
           | Circle of point * float;;	(* point is the center, float is radius *)

type peano = Zero | Succ of peano

*)

type sign = Positive | Negative | Zero

let signOf n =
    match n with
    | 0 -> Zero
    | _ when n>0 -> Positive
    | _ -> Negative

type point = Catesian of (float * float) | Polar of (float*float)

let negate p =
    match p with
    | Catesian(x,y) -> Catesian(-.x,-.y)
    | Polar (rho,theta) -> Polar(rho, theta +. 180. )


let toPolar p = 
    match p with
    | Polar _ -> p
    | Catesian(x,y) -> Polar( sqrt(x*.x +. y*.y), atan2 y x)

(*type 'a option = None | Some of 'a *)

let safeDiv (x,y) = 
    if y = 0 then None else Some(x/y)

(* :: Cons *)

type intlist = Nil | Cons of (int * intlist )

let rec length l = 
    match l with 
    | Nil -> 0
    | Cons(_,rest) -> 1 + length rest 

(* data only in internal nodes *)
type binaryTree = Leaf | Node of int * binaryTree * binaryTree 

(* data only at leaves

type  binaryTree = Leaf of int | Node of binarytree * binarytree

 * *)

(* tree -> list of elements *)
let rec inorder tree = 
    match tree with
    | Leaf -> []
    | Node(n,left,right) ->  ( inorder left ) @ [n] @ (inorder right )


(* Peano numbers *)
(*
In Peano arithmetic we have 2 symbols, Zero and Succ
which we use to represent all numbers 
Succ is a constructor representing the successor relation
So, Succ(Zero ) is the peano equivalent of  decimal 1.
Succ(Succ(Succ(Zero))) == 3 
 * *)


type peano = Zero | Succ of peano

(*convert peano to integer*)
let rec pToI p = 
    match p with
    | Zero -> 0
    | Succ(q) -> 1 + (pToI q )

(*convert integer to peano *)
let rec iToP i = 
    match i with
    | 0 -> Zero
    | _ -> Succ( iToP (i-1 ))

(*add 2 peano numbers and produce their sum as a peano number *)
let rec add (p1,p2) = 
    match p1 with
    | Zero -> p2
    | Succ(q) -> Succ(  add( q, p2 ) ) 


(* Multiply 2 peano numbers and produce their sum as a peano number *)
let rec multiply ((p1, p2) :peano * peano) :peano =
  match p1 with
    Zero    -> Zero
  | Succ(q) -> add (p2, multiply (q, p2))



(* functions *)

(* functions are *first-class*:
     - they are full-fledged expressions in the language
     - you can pass them to other functions
     - you can return them from other functions
*)

let square x = x*x

let toThe4th x = square(square x)  

let fourthRoot x = sqrt(sqrt x)  

(* twice is a *higher-order* function:
   function that takes another function as an argument *)   
let twice(f,x) = f(f x)



let rec range from till step = 
    if from <= till then from::(range (from + step) till step )
    else []

let rec simpleRange n = 
    range 1 n 1 


(*input: list of integers
 * for each element:
     * if x is even return x * x
     * otherwise return x 
     *
    * *)

let squareHelper i = 
    if i mod 2 == 0 then
        i*i
    else 
        i

let mapSquareEvenNumbers1 l = 
    List.map squareHelper l

let mapSquareEvenNumbers l = 
    List.map (fun x-> if x mod 2 == 0 then x*x else x ) l

let isPrime n = 
    let rec isPrimeHelper (n,i) =
        match n with 
        | 1 -> false
        | 2 -> true 
        | _ when n = i -> true
        | _ when (n mod i = 0 ) -> false 
        | _ -> isPrimeHelper( n, i+1 )
    in
    isPrimeHelper(n,2)

(*returns a list of all primes till number n *)
let primesGet n = 
    List.filter (fun x -> isPrime x )  (simpleRange n )

(*l1 and l2 are lists of equal length
 * returns a list of pairs
 * l1= ["a";"b"] l2 = [7;9] -> [("a",7);("b",9)]
 *
 * *)

let rec zip l1 l2 = 
    match (l1,l2) with 
    | ([],[]) -> [] 
    | (h1::t1,h2::t2) -> (h1,h2) :: ( zip t1 t2 )

(*Implement List.mapi using zip, List.map*)
let rec list_mapi f list =
  List.map f (zip (range 0 ((List.length list) - 1) 1) list);;

let list_mapi f list =
  let rec aux f idx = function
  [] -> []
  | x::xs -> (f idx x)::(aux f (idx+1) xs )
in
  aux f 0 list;;


