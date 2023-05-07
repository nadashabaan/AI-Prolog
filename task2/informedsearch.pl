% Define the main predicate to solve the dominoes puzzle
dominoes_informed_search(M, N, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, Result) :-
    % Initialize the start and goal states
    initial_state(M, N, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, Start),
    goal_state(M, N, Goal),
    % Perform the A* search to find the path from start to goal
    astar_search(Start, Goal, Result).


empty_board(M, N, Board) :-
    length(Board, M),
    maplist(length_list(N), Board).

length_list(Len, List) :-
    length(List, Len),
    maplist(=(0), List).
place_bombs(Board, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, BoardWithBombs) :-
    length(Board, NumRows),
    length([_|Board], NumCols), % assuming all rows have the same length
    numlist(1, NumRows, Rows),
    numlist(1, NumCols, Cols),
    maplist(put_bomb(Board, Bomb1_Row, Bomb1_Col), Rows, Board1),
    maplist(put_bomb(Board1, Bomb2_Row, Bomb2_Col), Rows, BoardWithBombs),
    !.

put_bomb(Board, Bomb_Row, Bomb_Col, Row, NewRow) :-
    put_bomb(Board, Bomb_Row, Bomb_Col, Row, 1, NewRow).

put_bomb([], _, _, [], _, []).
put_bomb([_|Board], Bomb_Row, Bomb_Col, [Row|Rows], Col, [Cell|NewRow]) :-
    (Row = Bomb_Row, Col = Bomb_Col ->
        Cell = 'B'
    ; otherwise ->
        Cell = 0
    ),
    NewCol is Col + 1,
    put_bomb(Board, Bomb_Row, Bomb_Col, Rows, NewCol, NewRow).


replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    J is I - 1,
    replace(T, J, X, R).
% Define the initial state of the puzzle
initial_state(M, N, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, State) :-
    % Create the empty board
    empty_board(M, N, Board),
    % Place the bombs on the board
    place_bombs(Board, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, BoardWithBombs),
    % Initialize the state with the board and no dominoes placed
    State = state(BoardWithBombs, [], 0).

% Define the goal state of the puzzle
goal_state(M, N, State) :-
    % Create the empty board
    empty_board(M, N, Board),
    % Initialize the state with the board and all dominoes placed
    count_empty_cells(Board, EmptyCells),
    Placed is EmptyCells // 2,
    State = state(Board, [], Placed).


% Define the heuristic function for A* search
heuristic(state(Board, _, Placed), H) :-
    % Count the number of empty cells on the board
    count_empty_cells(Board, EmptyCells),
    % Calculate the maximum number of dominoes that can be placed on the board
    MaxPlaced is EmptyCells // 2,
    % Calculate the minimum distance between the bombs and the empty cells
    distance_to_empty_cells(Board, EmptyCells, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, Distance),
    % Calculate the heuristic as the difference between the maximum number of
    % dominoes that can be placed and the number of dominoes already placed
    % minus the distance to the empty cells
    H is MaxPlaced - Placed - Distance.
distance_to_empty_cells(Board, EmptyCells, Bomb1_Row, Bomb1_Col, Bomb2_Row, Bomb2_Col, Distance) :-
    % Find the positions of the bombs on the board
    nth1(Bomb1_Row, Board, Row1),
    nth1(Bomb1_Col, Row1, 'B'),
    nth1(Bomb2_Row, Board, Row2),
    nth1(Bomb2_Col, Row2, 'B'),
    % Find the positions of the empty cells on the board
    findall([Row, Col], (nth1(Row, Board, RowList), nth1(Col, RowList, 0)), EmptyCellPositions),
    % Calculate the Manhattan distance between each bomb and each empty cell
    findall(D1, (member([Row, Col], EmptyCellPositions), D1 is abs(Row - Bomb1_Row) + abs(Col - Bomb1_Col)), Distances1),
    findall(D2, (member([Row, Col], EmptyCellPositions), D2 is abs(Row - Bomb2_Row) + abs(Col - Bomb2_Col)), Distances2),
    % Take the minimum distance between each bomb and the empty cells
    min_list(Distances1, MinDistance1),
    min_list(Distances2, MinDistance2),
    % Take the sum of the minimum distances and multiply by 2, since we are looking for the
    % minimum distance between the two bombs and the empty cells
    Distance is (MinDistance1 + MinDistance2) * 2.

% Define the A* search algorithm
astar_search(State, Goal, Result) :-
    % Initialize the open list with the start state
    empty_heap(Open),
    add_to_heap(Open, 0, State, OpenWithStart),
    % Initialize the closed list as empty
    empty_assoc(Closed),
    % Call the recursive A* search helper predicate
    astar_search_helper(OpenWithStart, Closed, Goal, Result).

% Define the recursive A* search helper predicate
astar_search_helper(Open, Closed, Goal, Result) :-
    % Check if the open list is empty
    ( is_heap(Open), heap_size(Open, 0) ->
        % If the open list is empty, the goal state was not found
        Result = []
    ; % Otherwise, continue the search
        % Get the state with the lowest f-value from the open list
        get_from_heap(Open, _, CurrentState, OpenWithoutCurrent),
        % Check if the current state is the goal state
        ( CurrentState = Goal ->
            % If the current state is the goal state, the search is complete
            % Return the path to the goal state as the result
            reconstruct_path(CurrentState, Closed, [], Result)
        ; % Otherwise, expand the current state and continue the search
            % Add the current state to the closed list
            put_assoc(CurrentState, Closed, 0, ClosedWithCurrent),
            % Get the successor states of the current state
            successor_states(CurrentState, SuccessorStates),
            % Add the successor states to the open list
            add_successor_states_to_open_list(SuccessorStates, CurrentState, OpenWithoutCurrent, OpenWithSuccessors),
            % Continue the search with the updated open and closed lists
            astar_search_helper(OpenWithSuccessors, ClosedWithCurrent, Goal, Result)
        )
    ).


% Define the successor states predicate
successor_states(state(Board, PlacedDominoes, PlacedCount), SuccessorStates) :-
% Get the dimensions of the board
length(Board, M),
length(Board, N),
% Loop over all possible domino placements
findall(state(NewBoard, [Domino | PlacedDominoes], NewPlacedCount),
( between(1, M, Row),
between(1, N, Col),
is_bomb(Board, Row, Col),
% Check if the domino can be placed horizontally
( can_place_horizontally(Board, Row, Col) ->
% Place the domino horizontally on the board
place_horizontally(Board, Row, Col, NewBoard),
% Update the placed count and add the domino to the placed list
NewPlacedCount is PlacedCount + 1,
Domino = domino(h, Row, Col),
% Check if the new state is valid and add it to the successor states
( is_valid_state(state(NewBoard, [Domino | PlacedDominoes], NewPlacedCount)) ->
SuccessorStates = [state(NewBoard, [Domino | PlacedDominoes], NewPlacedCount) | SuccessorStates]
; SuccessorStates = SuccessorStates
)
; % Check if the domino can be placed vertically
can_place_vertically(Board, Row, Col) ->
% Place the domino vertically on the board
place_vertically(Board, Row, Col, NewBoard),
% Update the placed count and add the domino to the placed list
NewPlacedCount is PlacedCount + 1,
Domino = domino(v, Row, Col),
% Check if the new state is valid and add it to the successor states
( is_valid_state(state(NewBoard, [Domino | PlacedDominoes], NewPlacedCount)) ->
SuccessorStates = [state(NewBoard, [Domino | PlacedDominoes], NewPlacedCount) | SuccessorStates]
; SuccessorStates = SuccessorStates
)
; SuccessorStates = SuccessorStates
)
),
SuccessorStates
).
successor_states(state(Board, Dominos, Placed), SuccessorStates) :-
    % Find all the empty cells on the board
    findall([Row, Col], (nth1(Row, Board, RowList), nth1(Col, RowList, 0)), EmptyCells),
    % Generate all possible domino placements in the empty cells
    findall([D1, D2], (
        member([Row1, Col1], EmptyCells),
        member([Row2, Col2], EmptyCells),
        (Row1 =:= Row2, Col2 is Col1 + 1 ; Col1 =:= Col2, Row2 is Row1 + 1),
        D1 = [Row1, Col1, Row2, Col2],
        D2 = [Row2, Col2, Row1, Col1],
        % Make sure the domino is not already placed
        \+ member(D1, Dominos),
        \+ member(D2, Dominos)
    ), DominosToPlace),
    % Generate all possible next states by placing each domino in the list
    findall(state(NewBoard, [Domino|Dominos], Placed1), (
        member([R1, C1, R2, C2], DominosToPlace),
        place_domino(Board, R1, C1, R2, C2, NewBoard),
        Placed1 is Placed + 1,
        % Make sure the new state is not already visited
        \+ member(state(NewBoard, _, _), Dominos)
    ), SuccessorStates). 


count_empty_cells(Board, Count) :-
    flatten(Board, FlatBoard),
    include(=(0), FlatBoard, EmptyCells),
    length(EmptyCells, Count).

place_domino(Board, R1, C1, R2, C2, NewBoard) :-
    % Make sure the cells are adjacent
    (R1 =:= R2, C2 is C1 + 1 ; C1 =:= C2, R2 is R1 + 1),
    % Make sure the cells are empty
    nth1(R1, Board, Row1),
    nth1(C1, Row1, 0),
    nth1(R2, Board, Row2),
    nth1(C2, Row2, 0),
    % Place the domino on the board
    replace(Row1, C1, 1, NewRow1),
    replace(Row2, C2, 1, NewRow2),
    replace(Board, R1, NewRow1, NewBoard1),
    replace(NewBoard1, R2, NewRow2, NewBoard).

% Define the can_place_horizontally predicate
can_place_horizontally(Board, Row, Col) :-
% Check that there are two empty cells to the right of the current cell
get_cell(Board, Row, Col, Cell),
Cell = empty,
get_cell(Board, Row, Col + 1, Cell1),
Cell1 = empty,
get_cell(Board, Row, Col + 2, Cell2),
Cell2 = empty.

% Define the can_place_vertically predicate
can_place_vertically(Board, Row, Col) :-
% Check that there are two empty cells below the current cell
get_cell(Board, Row, Col, Cell),
Cell = empty,
get_cell(Board, Row + 1, Col, Cell1),
Cell1 = empty,
get_cell(Board, Row + 2, Col, Cell2),
Cell2 = empty.


% Define the place_horizontally predicate
place_horizontally(Board, Row, Col, NewBoard) :-
    % Check that there are two empty cells to the right of the current cell
    get_cell(Board, Row, Col, Cell),
    Cell = empty,
    get_cell(Board, Row, Col + 1, Cell1),
    Cell1 = empty,
    get_cell(Board, Row, Col + 2, Cell2),
    Cell2 = empty,
    % Place the ship on the board
    set_cell(Board, Row, Col, occupied),
    set_cell(Board, Row, Col + 1, occupied),
    set_cell(Board, Row, Col + 2, occupied),
    % Create the new board
    copy_board(Board, NewBoard).

% Define the place_vertically predicate
place_vertically(Board, Row, Col) :-
% Check that there are two empty cells below the current cell
get_cell(Board
, Row, Col, Cell),
Cell = empty,
get_cell(Board, Row + 1, Col, Cell1),
Cell1 = empty,
get_cell(Board, Row + 2, Col, Cell2),
Cell2 = empty,
% Place the ship on the board
    set_cell(Board, Row, Col, occupied),
    set_cell(Board, Board, Row + 1, Col, occupied),
    set_cell(Board, Row + 2, Col, occupied),
    % Create the new board
    copy_board(Board, NewBoard).      