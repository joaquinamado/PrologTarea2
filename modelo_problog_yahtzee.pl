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


