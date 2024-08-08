different([],L,[]).
different([X|L],L1,[X|L2]):- \+ member(X,L1),!,different(L,L1,L2).
different([X|L],L1,L2):- member(X,L1),different(L,L1,L2).

member(X,[X|_]).
member(X,[_|T]):- member(X,T).

subsum([],0,[]).
subsum([X|L],Sum,[X|L1]):- Sum1 is Sum - X,subsum(L,Sum1,L1).
subsum([X|L],Sum,L2):- subsum(L,Sum,L2).

formula:- I is 1,write('Enter the formula number: '),read(N),nl, process(I,N).
process(11,_):- !.
process(I,N):- C is I * N,write(I),write(' X '),write(N),write(' = '),write(C),I1 is I+1,nl,process(I1,N).
		   
move(1,S,I,T):- write(S),tab(1),write('to'),tab(1),write(T),nl.
move(N,S,I,T):- N>0,N1 is N-1,move(N1,S,T,I),move(1,S,I,T),move(N1,I,S,T).


merge([],[],[]):-!.
merge(L,[],L):-!.
merge([],L,L):-!.
merge(L1,L2,L3):- L1 = [X1|T1],L2 = [X2|T2],L3 = [X1|T3],X1=<X2,merge(T1,L2,T3).
merge(L1,L2,L3):- L1 = [X1|T1],L2 = [X2|T2],L3 = [X2|T3],X1>X2,merge(L1,T2,T3).

split_list([],[],[]):-!.
split_list([X],[X],[]):-!.
split_list(L,L1,L2):- length_list(L,Len),Len>1,D is Len//2,length_list(L1,D),concat(L1,L2,L).

concat([],[],[]).
concat(L,[],L).
concat([],L,L).
concat(L1,L2,L):- L1=[X|T],L=[X|T1],concat(T,L2,T1).

length_list([],0).
length_list(L,N):-L=[H|T],length_list(T,N1),N is N1+1.

merge_sort([],[]):-!.
merge_sort([X],[X]):-!.
merge_sort(L,SL):-split_list(L,L1,L2),merge_sort(L1,SL1),merge_sort(L2,SL2),merge(SL1,SL2,SL).

aol([],[]):-!.
aol([X],[X]):-!.
aol(L,SL):-smallest(X,L),del(X,L,L1),aol(L1,SL1),SL = [X|SL1].

smallest(X,[X]):-!.
smallest(X,L):- L=[X1|L1],smallest(X2,L1),(X1=<X2,!,X is X1;X is X2).

del(X,[X],[]):-!.
del(X,L,L1):- L = [X|L1].
del(X,L,L1):- L = [Y|T1],del(X,T1,T),L1 = [Y|T].

grade:-write('Enter the marks: '),read(N),write('Grading of the marks is '),grading(N).
grading(N):-N>=80,!, write('A').
grading(N):-N>=65,!, write('B').
grading(N):-N>=50,!, write('C').
grading(N):-N>=35,!, write('D').
grading(N):-N>=80,!, write('A').
grading(N):- write('F').

parallel(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)):-Slope1 is (Y2-Y1)/(X2-X1),Slope2 is (Y4-Y3)/(X4-X3),
Slope1=:=Slope2.

perpendicular(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)):-Slope1 is (Y2-Y1)/(X2-X1),Slope2 is (Y4-Y3)/(X4-X3),
Scope1 * Scope2=:= -1 .

line_length(point(X1,Y1),point(X2,Y2),Len):-Len is sqrt((X2-X1)^2+(Y2-Y1)^2).

area_triangle(point(X1,Y1),point(X2,Y2),point(X3,Y3),Area):-Area is 0.5*((X2-X1)*(Y2+Y1)+(X3-X2)*(Y3+Y2)-(X3-X1)*(Y3+Y1)).  /*abs(X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)).*/

parallelogram(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)):- Scope1 is (Y2-Y1)/(X2-X1),Scope2 is (X3-X4)/(Y3-Y4),
Scope1 =:= Scope2,Scope3 is (Y1-Y4)/(X1-X4),Scope4 is (Y3-Y2)/(X3-X2),Scope3=:=Scope4.

square(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)):-
parallelogram(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)),
line_Length(point(X1,Y1),point(X2,Y2),Len1),
line_length(point(X3,Y3),point(X4,Y4),Len2),
Len1=:=Len2,
perpenticular(point(X1,Y1),point(X2,Y2),point(X3,Y3),point(X4,Y4)).

trans(0,[X|_],[]).
trans(1,[X|_],[X]).
trans(N,L,T):-length_list(L,Len),N=<Len,L = [H|T],N1 is N-1,trans(N1,T,T1),concat(H,T1,T).
translate(N1,N2,L,L1):-N1<N2,length_list(L,Len),N2=<Len,N is N1-1,trans(N,L,L2),trans(N2,L,L3),concat(L2,L1,L3).

add_last(X,[],[X]).
add_last(X,L,L1):- L = [Y|T1], L1 = [Y|T2],add_last(X,T1,T2).

list_sum([],0).
list_sum([X],X).
list_sum(L,Sum):-L= [X|L1],list_sum(L1,Sum1),Sum is X+Sum1.

list_mean(L,Mean):-list_sum(L,Sum),length_list(L,N),Mean is Sum / N.
list_mean([],0).

max([],0):-!.
max([X],X):-!.
max(L,M):-L=[H|T],max(T,M1),(H>=M1,!,M is H;M is M1).

between(N1,N2,N1):-N1=:=N2.
between(N1,N2,N):-N1<N2,NN is N1+1,between(NN,N2,N).

printS(1):-write('*'),!.
printS(N):-N1 is N-1,write('*'),printS(N1).
printStar(N):-N1 is 2*N-1,printS(N1).
printLine(L,N):- T is 50-N,tab(T),printStar(N),N1 is N+1,N1=<L,nl,printLine(L,N1),!;true.

multiple:-write('multiplication formula number is '),read(N),I is 1,process(I,N).
process(11,_):-!.
process(I,N):- C is I * N,write(N),write('X'),write(I),write('='),write(C),nl,I1 is I+1,process(I1,N).

qs([],[]).
qs([X|T],SL):-split(X,T,Small,Big),qs(Small,SS),qs(Big,SB),conc(SS,[X|SB],SL).
split(X,[],[],[]).
split(X,[Y|T],[Y|Small],Big):- gt(X,Y),!,split(X,T,Small,Big).
split(X,[Y|T],Small,[Y|Big]):-split(X,T,Small,Big).

star(1):-write('*'),!.
star(N):-write('*'),N1 is N-1,star(N1).
printC(N):-tab(50),star(N).
line(L,N):-printC(N),N1 is N+1,N1=<L,nl,line(L,N1),!;true.

squar(0,0).
squar(1,1).
squar(N,SN):-SN is N * N.

/*----------------------------------*/

circle:-write('Enter the radius of circle: '),read(R),write('Area of the circle: '),area_of_circle(R).
area_of_circle(0):-!.
area_of_circle(R):- A is 3.14 * R * R,write(A).

/*-------------------------------------------------------------------------------------------------------*/
line_length(X1,Y1,X2,Y2,Len):- Len is sqrt((X2-X1)^2+(Y2-Y1)^2).

/*--------------------------------------------------------------------------------------*/

gradient(X1,Y1,X2,Y2,G):- G is (Y2-Y1)/(X2-X1).

/*------------------------------------------------------------------------*/

perpendicular(X1,Y1,X2,Y2,X3,Y3,X4,Y4):-Slope1 is (Y2-Y1)/(X2-X1),
Slope2 is (Y4-Y3)/(X4-X3),Slope1 * Slope2 =:= -1 .

/*----------------------------------------------------------------------------------------------------*/

star(0):-!.
star(1):-write('*'),!.
star(N):-write('*'),N1 is N-1,star(N1).
printS(N):-tab(50),star(N).  

/*-----------------------------------------------------------------------------*/

line(L,N):-N>0,N=<L,printS(N),nl,N1 is N+1,line(L,N1).

/*-------------------------------------------------------------------*/

printC(N):-N1 is 2*N-1,star(N1).
printLine(L,N):- T is 50-N,tab(T),N>0,N=<L,printC(N),nl,N1 is N+1,printLine(L,N1).

/*---------------------------------------------------------------------------------------------*/

pline(L,N):-N>0,N=<L,printS(L),nl,L1 is L-1,pline(L1,N).

/*-----------------------------------------------------------------------------------*/
stars(0):-!.
stars(1):-write(' * '),!.
stars(N):-write(' * '),tab(1),N1 is N-1,star(N1).
printStars(N):-tab(50),stars(N). 
lline(L,N):- T is 50-N,tab(T),N>0,N=<L,printStars(N),nl,N1 is N+1,lline(L,N1).


male(bertram).
male(percival).
female(lucinda).
female(camilla).
pair(X,Y):-male(X),female(Y).

/*---------------------------------------------------*/
fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,X):- N>1,N1 is N-1,N2 is N-2,
fibonacci(N1,F1),fibonacci(N2,F2),X is F1+F2.

/*------------------------------------------------------*/
remove_duplicate([],[]).
remove_duplicate(L,L1):- L=[H|T],remove_duplicate(T,L2),union([H],L2,L1).

union([],[],[]):-!.
union(L,[],L):-!.
union([],L,L):-!.
union([X|L1],L2,[X|L3]):- \+member(X,L2),union(L1,L2,L3).
union([X|L1],L2,L3):- member(X,L2),union(L1,L2,L3).

/*--------------------------------------------------------------------*/
max([],0):-!.
max([X],X):-!.
max(L,M):-L=[H|T],max(T,M1),(H>=M1,!,M is H;M is M1).

/*------------------------------------------------------------------*/
different([],[],[]):-!.
different(L,[],[]):-!.
different([],L,[]):-!.
different([X|L1],L2,[X|L3]):- \+member(X,L2),different(L1,L2,L3).
different([X|L1],L2,L3):- member(X,L2),different(L1,L2,L3).

/*--------------------------------------------------------------------*/

move(1,S,I,T):- write(S),tab(2),write('to'),tab(2),write(T),nl,!.
move(N,S,I,T):- N>1,N1 is N-1,move(N1,S,T,I),move(1,S,I,T),move(N1,I,S,T).

rotate([],_,[]).
rotate(L,N,RL):-N>0,length(L,Len),N=<Len,
rotateList(L,N,RL).

rotate(L,N,RL):-N<0,length(L,Len),N1 is Len+N,
rotateList(L,N1,RL).

rotateList(L,0,L).
rotateList(L,N,RL):-L=[H|T],N>0,N1 is N-1,append(T,[H],L1),
rotateList(L1,N1,RL).

/*---------------------------------------------------------------------------------------*/
list_contain([]):-fail.
list_contain([H|_]):-number(H),!.
list_contain([_|T]):-list_contain(T).

/*----------------------------------------------------------------------------------------*/
remove_parenthesis([],[]).
remove_parenthesis([H|T],L):-is_list(H),remove_parenthesis(H,LH),
remove_parenthesis(T,LT),append(LH,LT,L).

remove_parenthesis([H|T],[H|L]):- \+is_list(H),remove_parenthesis(T,L).

super(daniel,lisa).
super(daniel,stewie).
super(lisa,peter).
super(lisa,marge).
super(lisa,maggie).
super(stewie,peter).
super(stewie,lois).
super(lois,meg).
super(peter,brian).
super(marge,clnis).
super(marge,bart).

jointly_supervised(X,Y,Z):-super(X,Z),super(Y,Z),X\=Y.

same_supervisor(X,Y1,Y2):-super(X,Y1),super(X,Y2),Y1\=Y2.

different_supervisor(Y1,Y2,X):-super(Z,Y1),super(M,Y2),
super(X,Z),super(X,M),Z\=M,Y1\=Y2.

/*--------------------------------------------------------------------------*/

male(fedrik).
male(zelenski).
male(prigozhin).
male(daniel).
male(thomas).
male(bose).

female(linda).
female(barbara).
female(ella).
female(elizabath).

parent(fedrik,ella).
parent(fedrik,elizabath).
parent(linda,ella).
parent(linda,elizabath).
parent(zelenski,daniel).
parent(barbara,daniel).
parent(prigozhin,thomas).
parent(ella,thomas).
parent(elizabath,bose).
parent(daniel,bose).

% rule
father_of(X,Y):-male(X),parent(X,Y).
mother_of(X,Y):-female(X),parent(X,Y).
grandfather_of(X,Y):-male(X),parent(X,Z),parent(Z,Y).
grandmother_of(X,Y):-female(X),parent(X,Z),parent(Z,Y).
sister_of(X,Y):-female(X),parent(Z,X),parent(Z,Y),X\=Y.
brother_of(X,Y):-male(X),parent(Z,X),parent(Z,Y),X\=Y.
aunt_of(X,Y):-parent(Z,Y),sister_of(X,Z).
uncle_of(X,Y):-parent(Z,Y),brother_of(X,Z).

grandparent(X,Y):-grandfather_of(X,Y);grandmother_of(X,Y).
grandchildren(X,Y):-parent(Y,Z),parent(Z,X).

/*------------------------------------------------------------------------*/
