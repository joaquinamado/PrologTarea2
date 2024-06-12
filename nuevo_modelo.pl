% Prolog
% Definir los posibles valores para cada dado
1/6::dado1(1);1/6::dado1(2);1/6::dado1(3);1/6::dado1(4);1/6::dado1(5);1/6::dado1(6).
1/6::dado2(1);1/6::dado2(2);1/6::dado2(3);1/6::dado2(4);1/6::dado2(5);1/6::dado2(6).
1/6::dado3(1);1/6::dado3(2);1/6::dado3(3);1/6::dado3(4);1/6::dado3(5);1/6::dado3(6).
1/6::dado4(1);1/6::dado4(2);1/6::dado4(3);1/6::dado4(4);1/6::dado4(5);1/6::dado4(6).
1/6::dado5(1);1/6::dado5(2);1/6::dado5(3);1/6::dado5(4);1/6::dado5(5);1/6::dado5(6).


yahtzee_prob :-
    dado1(Dado),
    dado2(Dado),
    dado3(Dado),
    dado4(Dado),
    dado5(Dado).

yahtzee_prob_dados(Dados, Prob) :- 
    subquery(yahtzee_prob, Prob, Dados).
yahtzee_prob_dados(_, Prob).

four_of_a_kind_prob :-
    (dado1(Dado), dado2(Dado), dado3(Dado), dado4(Dado));
    (dado1(Dado), dado2(Dado), dado3(Dado), dado5(Dado));
    (dado1(Dado), dado2(Dado), dado4(Dado), dado5(Dado));
    (dado1(Dado), dado3(Dado), dado4(Dado), dado5(Dado));
    (dado2(Dado), dado3(Dado), dado4(Dado), dado5(Dado)).

four_of_a_kind_prob_dados(Dados, Prob) :-
    subquery(four_of_a_kind_prob, Prob, Dados).
four_of_a_kind_prob_dados(_, Prob).

three_of_a_kind_prob :-
    (dado1(Dado), dado2(Dado), dado3(Dado));
    (dado1(Dado), dado2(Dado), dado4(Dado));
    (dado1(Dado), dado2(Dado), dado5(Dado));
    (dado1(Dado), dado3(Dado), dado4(Dado));
    (dado1(Dado), dado3(Dado), dado5(Dado));
    (dado1(Dado), dado4(Dado), dado5(Dado));
    (dado2(Dado), dado3(Dado), dado4(Dado));
    (dado2(Dado), dado3(Dado), dado5(Dado));
    (dado2(Dado), dado4(Dado), dado5(Dado));
    (dado3(Dado), dado4(Dado), dado5(Dado)).

three_of_a_kind_prob_dados(Dados, Prob) :-
    subquery(three_of_a_kind_prob, Prob, Dados).
three_of_a_kind_prob_dados(_, Prob).

full_house_prob :- 
    (dado1(Dado1), dado2(Dado1), dado3(Dado1), dado4(Dado2), dado5(Dado2));
    (dado1(Dado1), dado2(Dado1), dado3(Dado2), dado4(Dado2), dado5(Dado1));
    (dado1(Dado1), dado2(Dado2), dado3(Dado2), dado4(Dado1), dado5(Dado1));
    (dado1(Dado2), dado2(Dado2), dado3(Dado1), dado4(Dado1), dado5(Dado1));
    (dado1(Dado2), dado2(Dado1), dado3(Dado1), dado4(Dado2), dado5(Dado1));
    (dado1(Dado2), dado2(Dado1), dado3(Dado2), dado4(Dado1), dado5(Dado1));
    (dado1(Dado1), dado2(Dado2), dado3(Dado1), dado4(Dado2), dado5(Dado1));
    (dado1(Dado2), dado2(Dado1), dado3(Dado1), dado4(Dado1), dado5(Dado2)).

full_house_prob_dados(Dados, Prob) :-
    subquery(full_house_prob, Prob, Dados).
full_house_prob_dados(_, Prob).


% Chequear que una lista sea una secuencia, 
% que todos los elementos sean consecutivos
es_sequencia([_]).
es_sequencia([1,3,4,5,6]).
es_sequencia([1,2,3,4,6]).
es_sequencia([X,Y|Rest]) :-
    es_sequencia([Y|Rest]),
    Y is X + 1.


small_straight_prob :- 
    dado1(Dado1), dado2(Dado2), dado3(Dado3), dado4(Dado4), dado5(Dado5),
    sort([Dado1, Dado2, Dado3, Dado4, Dado5], Sorted),
    (length(Sorted, 4);
    length(Sorted, 5)),
    es_sequencia(Sorted).


small_straight_prob_dados(Dados, Prob) :-
    subquery(small_straight_prob, Prob, Dados).
small_straight_prob_dados(_, Prob).

large_straight_prob :- 
    dado1(Dado1), dado2(Dado2), dado3(Dado3), dado4(Dado4), dado5(Dado5),
    sort([Dado1, Dado2, Dado3, Dado4, Dado5], Sorted),
    length(Sorted, 5),
    es_sequencia(Sorted).

large_straight_prob_dados(Dados, Prob) :-
    subquery(large_straight_prob, Prob, Dados).
large_straight_prob_dados(_, Prob).


query(four_of_a_kind_prob_dados([dado1(3),dado2(3)], Prob)).