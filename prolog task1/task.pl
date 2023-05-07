
member(X, [X|_]).
member(X, [_|Tail]):-
member(X, Tail).

add([], List, List).
add([Head|Tail], List, [Head|Result]) :-
add(Tail, List, Result).


%task 1
is_friend(X,Y):-friend(X,Y),!;friend(Y,X),!.

%task 2
friendList(Person, FriendsList) :-
    friendListHelper(Person, [], FriendsList).

friendListHelper(Person, Acc, FriendsList) :-
    friend(Person, Friend),
    \+ member(Friend, Acc),!,
    friendListHelper(Person, [Friend|Acc], FriendsList).

friendListHelper(_, FriendsList, FriendsList).

%task 3

countFriend([],0).
countFriend([_|Tail], N):-
    countFriend(Tail, N1),N is N1+1.

friendListCount(Person, N):-
    friendList(Person, FriendsList),
    countFriend(FriendsList,N).

%task 4
peopleYouMayKnow(Person,Possiblefriend):-
  friend(Person,Friend),
  (friend(Friend,Possiblefriend);friend(Possiblefriend,Friend)),
  not(friend(Person,Possiblefriend)),Possiblefriend \=Person.
%task 5

peopleYouMayKnow5(Person, N, SuggestedFriend) :-
     mutualFriends(Person, MutualFriends),
    friendOfMutualFriends(MutualFriends, FriendOfMutualFriends),
    countOccurrences(FriendOfMutualFriends, Occurrences),
    selectFriend(Occurrences, N, SuggestedFriend),
    \+ friend(Person, SuggestedFriend),!.

% Define the mutualFriends predicate
mutualFriends(Person, MutualFriends) :-
    mutualFriends(Person, [], MutualFriends).
mutualFriends(Person, Acc, MutualFriends) :-
    friend(Person, Friend),
    \+ member(Friend, Acc),
    mutualFriends(Person, [Friend|Acc], MutualFriends).
mutualFriends(_, MutualFriends, MutualFriends).

% Define the friendOfMutualFriends predicate
friendOfMutualFriends([], []).
friendOfMutualFriends([H|T], FriendOfMutualFriends) :-
    friend(H, Friend),
    \+ member(Friend, [H|T]),
    friendOfMutualFriends(T, Rest),
    add([Friend], Rest, FriendOfMutualFriends).

% Define the countOccurrences predicate
countOccurrences([], []).
countOccurrences([H|T], [(H, Count)|Result]) :-
    count(H, [H|T], Count),
    countOccurrences(T, Result).

% Define the count predicate
count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, M),
    N is M + 1.
count(X, [H|T], N) :-
    X \= H,
    count(X, T, N).

% Define the selectFriend predicate
selectFriend([], _, _) :- fail.
selectFriend([(Friend, Occurrences)|T], N, Friend) :-
    Occurrences >= N .
selectFriend([(Friend, Occurrences)|T], N, SuggestedFriend) :-
    Occurrences < N,
    selectFriend(T, N, SuggestedFriend).


%task 6

get_friends(Person, FriendsList) :-
    friends(Person, [], FriendsList).

friends(Person, Acc, FriendsList) :-
    (friend(Person, Friend);friend(Friend,Person)),
    \+ member(Friend, Acc),!,
    friends(Person, [Friend|Acc], FriendsList).
friends(_, FriendsList, FriendsList).

peopleYouMayKnowList(Person, SuggestedFriend) :-
    get_friends(Person,FriendList),
    mutual_friend(FriendList,List,Output),
    remove_duplicates(Output,Res),
    remove_friends(Person,Res,Newlist),
    delete(Person,Newlist,Fres),
    SuggestedFriend=Fres,!.

mutual_friend([],X,X).

mutual_friend([H|T],L,Output):-
    get_friends(H,FriendList),
    add(L,FriendList,Res),
    mutual_friend(T,Res,Output).


remove_duplicates([], []).

remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !,
    remove_duplicates(Tail, Result).

remove_duplicates([Head | Tail], [Head | Result]) :-
    remove_duplicates(Tail, Result).
remove_friends(_, [], []).
remove_friends(Person, [Item|Rest], NewList) :-
    friend(Person, Item),
    remove_friends(Person, Rest, NewList).
remove_friends(Person, [Item|Rest], [Item|NewList]) :-
    not(friend(Person, Item)),
    remove_friends(Person, Rest, NewList).

delete(X, [X|Y], Y).
delete(X, [Y|Tail1], [Y|Tail2]):-
delete(X, Tail1, Tail2).

%Bonus Task 
% Rule to check if two people have an indirect friend in common
haveIndirectFriend(A, B) :- 
    friend(A, X),
    friend(X, Y),
    friend(Y, B),
    not(friend(X, B)).


% Rule to suggest possible friends if there is an indirect relation
peopleYouMayKnow_indirect(A, X) :- 
    haveIndirectFriend(A, X),
    not(friend(A, X)), % Ensure there is no direct friendship between A and X
    X \= A. % Ensure X is not A

