create_empty_board(M, N,(Bx1,By1),(Bx2,By2),Fboard) :-
    length(Board, M),
    maplist(same_length(['.'|_]), Board),
    maplist(fill_row_with_dots(N), Board),
    replace(Board,Bx1,By1,'B',Nboard),
    replace(Nboard,Bx2,By2,'B',Fboard),!.


fill_row_with_dots(N, Row) :-
    length(Row, N),
    maplist(=(.), Row).

find_empty_cell(Board,(X, Y)) :-
    nth1(X, Board, RowList),
    nth1(Y, RowList, '.').


replace( [L|Ls] , 1 , Y , Z , [R|Ls] ) :-
    replace_column(L,Y,Z,R) .


replace( [L|Ls] , X , Y , Z , [L|Rs] ) :-
  X > 1 ,
  X1 is X-1 ,
  replace( Ls , X1 , Y , Z , Rs )
  .

replace_column( [_|Cs] , 1 , Z , [Z|Cs] ) .
replace_column( [C|Cs] , Y , Z , [C|Rs] ) :-
  Y > 1 ,
  Y1 is Y-1 ,
  replace_column( Cs , Y1 , Z , Rs ).


domino(M,N,B1,B2,G):-
    create_empty_board(M,N,B1,B2,Board),
    path([Board],[Board],G).



path([State|_],_,State):-
    \+try_place_domino(State,NBoard),!.



path([State|Open],Closed,G):-
    expand_state(State,Closed, Successors),
    append(Open,Successors,NewOpen),
    path(NewOpen,[State|Closed],G).




expand_state(State,Closed, Successors) :-
  findall(Ns,try_place_dominos(State,Closed,Ns),Successors).

try_place_dominos(State,Closed,Ns):-
    try_place_domino(State,Ns),
    \+member(Ns,Closed).

try_place_domino(Board, Newboard) :-
  find_empty_cell(Board,(X,Y)),
  length(Board,N),
  Y1 is Y + 1,
  Y1 < N+1,
  nth1(X, Board, Row),
  nth1(Y1, Row, '.'),
  replace(Board,X,Y,0,Nboard),
  replace(Nboard,X,Y1,0,Newboard).



try_place_domino(Board,Newboard) :-
  find_empty_cell(Board,(X,Y)),
  length(Board,N),
  X1 is X + 1,
  X1 < N+1,
  nth1(X1, Board, Row),
  nth1(Y, Row, '.'),
  replace(Board,X,Y,1,Nboard),
  replace(Nboard,X1,Y,1,Newboard).