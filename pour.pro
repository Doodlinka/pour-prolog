
% Олешко Дар'я Олексіївна - переливання
% TODO:
% - BFS
% - clean predicate returns move list

% 1. define pouring from one bucket to another
% ++, ++, ++, -, - because no equations, can be used to check
% but it's internal use

% case 1: not enough to fill dst
do_pour(DstCap, SrcBefore, DstBefore, SrcAfter, DstAfter) :-
    Total is SrcBefore + DstBefore,
    DstCap >= Total,
    SrcAfter is 0,
    DstAfter is Total.

% case 2: enough to fill dst
do_pour(DstCap, SrcBefore, DstBefore, SrcAfter, DstAfter) :-
    Total is SrcBefore + DstBefore,
    DstCap < Total,
    SrcAfter is Total - DstCap,
    DstAfter is DstCap.


% 2. define legal moves between states
% the last parameter describes if it's usable in a closed system

% ++, -, ++ because no arithmetic, can check but is for internal use

% legal move 1 (any variant): pour from one bucket to another
% select takes a list, returns some element of that list and the rest of the
% list. not the TAIL, the REST, to the left and right
% we do need to check both (a, b) and (b, a), and we need to stitch the list
% back together later, so this is good actually
move(State, Next, _) :-
    % pick any bucket as source
    select(b(SrcCur, SrcCap), State, Rest1), 
    % but don't bother pouring from empty
    SrcCur > 0, 
    % pick any bucket besides source as destination
    select(b(DstCur, DstCap), Rest1, Rest2),
    % don't bother pouring to full 
    DstCur < DstCap, % then pick a destination and don't bother pouring to full
    % get the buckets after pouring from src to dst
    do_pour(DstCap, SrcCur, DstCur, NewSrc, NewDst),
    % put the buckets back into the list
    NextTemp = [b(NewSrc, SrcCap), b(NewDst, DstCap) | Rest2],
    % sort the list because we messed up its order
    msort(NextTemp, Next).

% legal move 2 (open systems only): fill bucket
move(State, Next, open) :-
    % pick one
    select(b(Cur, Cap), State, Rest),
    % not an empty one
    Cur < Cap,
    % return it full
    NextTemp = [b(Cap, Cap) | Rest],
    % don't mess up the order
    msort(NextTemp, Next).

% legal move 3 (open systems only): empty bucket
move(State, Next, open) :-
    % pick one
    select(b(Cur, Cap), State, Rest), 
    % not a full one
    Cur > 0,
    % return it empty
    NextTemp = [b(0, Cap) | Rest],
    % don't mess up the order
    msort(NextTemp, Next).


% goal check, selected based on the goal passed in
% ++, ++ because there's endless ways to get from somewhere to somewhere,
% it's just a true/false thing, literally a predicate

% number goal
is_goal(State, TargetNum) :-
    number(TargetNum),
    member(b(TargetNum, _), State).

% exact state goal
is_goal(State, TargetState) :-
    is_list(TargetState),
    State = TargetState.


% 3. search for the thing, sorting buckets every step of the way
% to make sure equivalent states are considered the same

% ++, ++, ++, ++, - because there's no equations and no one thing to work backwards to
% checking might be possible though
% base case calls the number variant if target is a variant, and exact if
% target is a list
bfs([[State, RevPath] | _], Target, _, _, Path) :-
    is_goal(State, Target),
    reverse(RevPath, Path),
    !. % don't look further, other "solutions" skip past the goal

bfs([[Current, Path] | Queue], Target, Visited, Mode, FinalPath) :-
    % find every possible next step from here (state and new path with the appended action)
    findall(
        [Next, [Next | Path]],
        (
            move(Current, Next, Mode), 
            \+ member(Next, Visited)
        ),
        Candidates
    ),
    % this takes just the states from Candidates without the paths
    extract_states(Candidates, NewStates),
    % update visited and queue
    append(Visited, NewStates, NewVisited),
    append(Queue, Candidates, NewQueue),
    % recurse
    bfs(NewQueue, Target, NewVisited, Mode, FinalPath).

% ++, - because no way to iinvent some arbitrary paths, and checking may
% be possible (but internal use)
extract_states([], []).
extract_states([[S, _]|T], [S|T2]) :- extract_states(T, T2).


% 4. define a usable interface
% note: I sort at every step to avoid a situation where the state
% doesn't match up with what we've been given because the bukets have
% been shuffled around
% and because keeping track of the changes is a PITA even with sorting,
% I defined the path though actions taken
% maybe it could be made to work, but I don't care

% ++, ++,, ?
% can solve, can check, can't make up arbitrary paths
% from somewhere to somewhere`

solve_num_open(Start, TargetNum, Path) :-
    msort(Start, S),
    bfs([[S, []]], TargetNum, [S], open, Path).

solve_open(Start, TargetState, Path) :-
    msort(Start, S),
    msort(TargetState, T),
    bfs([[S, []]], T, [S], open, Path).

solve_num_closed(Start, TargetNum, Path) :-
    msort(Start, S),
    bfs([[S, []]], TargetNum, [S], closed, Path).

solve_closed(Start, TargetState, Path) :-
    msort(Start, S),
    msort(TargetState, T),
    bfs([[S, []]], T, [S], closed, Path).
