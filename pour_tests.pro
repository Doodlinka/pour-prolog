:- consult("pour.pro").
% :- use_module(library(plunit)).
% :- set_test_options([silent(false)]).


run_tests :-
    write('=== RUNNING TESTS ==='), nl, nl,
    test_die_hard,
    test_closed_system,
    test_impossible,
    test_sanity_check,
    test_exact_state,
    write('=== ALL TESTS COMPLETED ==='), nl.

% --- ТЕСТ 1: Класика (Міцний горішок) ---
% 5л і 3л, отримати 4л. Відкрита система.
% Очікується: 6 кроків (найкоротший шлях).
test_die_hard :-
    write('[TEST 1] Die Hard (5L, 3L -> 4L, Open)'), nl,
    Start = [b(0,5), b(0,3)],
    Target = 4,
    (solve_num_open(Start, Target, Path) ->
        length(Path, Len),
        write('  Result: SUCCESS'), nl,
        write('  Path Length: '), write(Len), write(' (Optimal is 6)'), nl,
        write('  Path: '), write(Path), nl
    ;
        write('  Result: FAILED (No solution found)'), nl
    ), nl.

% --- ТЕСТ 2: Закрита система ---
% 8л (повне), 5л (пусте), 3л (пусте). Отримати 4л.
% Очікується: 7 кроків (для отримання двох четвірок [4, 4, 0]).
test_closed_system :-
    write('[TEST 2] Closed System (8L, 5L, 3L -> 4L)'), nl,
    Start = [b(8,8), b(0,5), b(0,3)],
    Target = 4,
    (solve_num_closed(Start, Target, Path) ->
        length(Path, Len),
        write('  Result: SUCCESS'), nl,
        write('  Path Length: '), write(Len), nl,
        write('  Path: '), write(Path), nl
    ;
        write('  Result: FAILED'), nl
    ), nl.

% --- ТЕСТ 3: Неможлива задача ---
% 2л і 4л (парні). Отримати 3л (непарне).
% Очікується: FAIL (false), але НЕ ЗАВИСАННЯ.
test_impossible :-
    write('[TEST 3] Impossible Case (2L, 6L -> 3L)'), nl,
    Start = [b(0,2), b(0,6)],
    Target = 3,
    write('  Searching... (should fail quickly)'), nl,
    (solve_num_open(Start, Target, _) ->
        write('  Result: ERROR (Found solution for impossible task!)'), nl
    ;
        write('  Result: PASS (Correctly failed)'), nl
    ), nl.

% --- ТЕСТ 4: Вже вирішено ---
% Починаємо зі станом, де вже є 4 літри.
% Очікується: порожній список дій [] або список з 0 елементів.
test_sanity_check :-
    write('[TEST 4] Already Solved Check'), nl,
    Start = [b(4,5), b(0,3)],
    Target = 4,
    solve_num_open(Start, Target, Path),
    write('  Path: '), write(Path), nl,
    (Path == [] -> write('  Result: PASS'), nl ; write('  Result: WARNING (Path not empty)')), nl.

% --- ТЕСТ 5: Конкретний стан (не число) ---
% З (0,5)(0,3) отримати (5,5)(3,3) - обидва повні.
test_exact_state :-
    write('[TEST 5] Target State (Full buckets)'), nl,
    Start = [b(0,5), b(0,3)],
    TargetState = [b(5,5), b(3,3)],
    solve_open(Start, TargetState, Path),
    write('  Path: '), write(Path), nl, nl.