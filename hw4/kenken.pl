/** plain_kenken statistics
 * Times              since start      since last
 *
 *   user   time       6.338 sec       6.338 sec
 *   system time       0.011 sec       0.011 sec
 *   cpu    time       6.349 sec       6.349 sec
 *   real   time      29.971 sec      29.971 sec
*/

/** kenken statistics 
 * Times              since start      since last
 *
 *   user   time       0.006 sec       0.006 sec
 *   system time       0.005 sec       0.005 sec
 *   cpu    time       0.011 sec       0.011 sec
 *   real   time      40.399 sec      40.399 sec
 */

run:-
	consult('kenken.pl').

/** transpose a matrix */
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


/** Check whether the constraints C are satisfied */
plain_satisfyConstraints(_,[]).
plain_satisfyConstraints(T,[H|Ts]):-
	plain_satisfyConstraint(T,H),
	plain_satisfyConstraints(T,Ts).

/** different cases for plain_satisfyConstraint */
plain_satisfyConstraint(T,+(Sum,PosLs)):-
	plain_addAll(Result,PosLs,T),
	Sum #= Result.

plain_satisfyConstraint(T,*(Product,PosLs)):-
	plain_multAll(Result,PosLs,T),
	Product #= Result.

plain_satisfyConstraint(T,-(Diff,Pos1,Pos2)):-
	get(Val1,Pos1,T),
	get(Val2,Pos2,T),
	(Diff #= Val1-Val2;
	Diff #= Val2-Val1).

plain_satisfyConstraint(T,/(Quotient,Pos1,Pos2)):-
	get(Val1,Pos1,T),
	get(Val2,Pos2,T),
	(Quotient #= Val1/Val2;
	Quotient #= Val2/Val1).

/** helper functions for plain_satisfyConstraint */
plain_addAll(Sum, [H|[]],T):-
	get(Val,H,T),
	Sum = Val.

plain_addAll(Sum,[H|Ts],T):-
	get(Val,H,T),
	plain_addAll(NewSum,Ts,T),
	Sum = Val + NewSum.


plain_multAll(Prod, [H|[]],T):-
	get(Val,H,T),
	Prod = Val.

plain_multAll(Prod,[H|Ts],T):-
	get(Val,H,T),
	plain_multAll(NewProd,Ts,T),
	Prod = Val * NewProd.

get(Val,Row-Col,T):-
	nth(Row,T,TheRow),
	nth(Col,TheRow,Val).

/** init matrix */
initMatrix(N, T) :-
	length(T, N),
	initRows(N, T).
	
initRows(_, []) .
initRows(N, [H | T]) :-
	length(H, N),
	fd_domain(H, 1, N),
	initRows(N, T).

/** label T */
label([]) .
label([H | T]) :-
	fd_labeling(H),
	label(T).

/** The main Program */
kenken(N,C,T):-
	initMatrix(N,T),
	plain_satisfyConstraints(T,C),
	maplist(fd_all_different, T),
	transpose(T, TT),
	maplist(fd_all_different, TT),
	label(T).


/** The following is the plain_kenken code,
  * which does NOT use GNU finite domain solver.
  * FDS is based on Arc Consistency algorithm.
  * By reducing the domain size that need to be searched,
  * FDS can help greatly boost efficiency.
  * We use the following code to see the difference.
*** */


/** Check whether the constraints C are satisfied */
plain_satisfyConstraints(_,[]).
plain_satisfyConstraints(T,[H|Ts]):-
	plain_satisfyConstraint(T,H),
	plain_satisfyConstraints(T,Ts).

/** different cases for plain_satisfyConstraint */
plain_satisfyConstraint(T,+(Sum,PosLs)):-
	plain_addAll(Result,PosLs,T),
	Sum =:= Result.

plain_satisfyConstraint(T,*(Product,PosLs)):-
	plain_multAll(Result,PosLs,T),
	Product =:= Result.

plain_satisfyConstraint(T,-(Diff,Pos1,Pos2)):-
	get(Val1,Pos1,T),
	get(Val2,Pos2,T),
	(Diff =:= Val1-Val2;
	Diff =:= Val2-Val1).

plain_satisfyConstraint(T,/(Quotient,Pos1,Pos2)):-
	get(Val1,Pos1,T),
	get(Val2,Pos2,T),
	(Quotient is Val1//Val2;
	Quotient is Val2//Val1).

/* helper functions for plain_satisfyConstraint */
plain_addAll(Sum, [H|[]],T):-
	get(Val,H,T),
	Sum = Val.

plain_addAll(Sum,[H|Ts],T):-
	get(Val,H,T),
	plain_addAll(NewSum,Ts,T),
	Sum = Val + NewSum.


plain_multAll(Prod, [H|[]],T):-
	get(Val,H,T),
	Prod = Val.

plain_multAll(Prod,[H|Ts],T):-
	get(Val,H,T),
	plain_multAll(NewProd,Ts,T),
	Prod = Val * NewProd.

/** init matrix */
plain_initMatrix(N, T) :-
	length(T, N),
	plain_initRows(N, T).
	
plain_initRows(_, []) .
plain_initRows(N, [H | T]) :-
	length(H, N),
	domainList(H, 1, N),
	all_different(H),
	plain_initRows(N, T).

/** replace fd_domain */
domain(L,L,_).
domain(Val,L,U):-
	N is L+1,
	N =< U,
	domain(Val,N,U).

domainList([],_,_).
domainList([H|T],Lower,Upper):-
	domain(H,Lower,Upper),
	domainList(T,Lower,Upper).


/** replace fd_all_different */
all_different(L) :-
	\+ (select(Val,L,R), memberchk(Val,R)).

/** The main Program */
plain_kenken(N,C,T):-
	plain_initMatrix(N,T),
	plain_satisfyConstraints(T,C),
	maplist(all_different, T),
	transpose(T, TT),
	maplist(all_different, TT).



/** kenken test case */
kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).

