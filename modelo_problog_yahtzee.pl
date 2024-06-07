% Prolog
% Definir los posibles valores para cada dado
1/6::dado1(1);1/6::dado1(2);1/6::dado1(3);1/6::dado1(4);1/6::dado1(5);1/6::dado1(6).
1/6::dado2(1);1/6::dado2(2);1/6::dado2(3);1/6::dado2(4);1/6::dado2(5);1/6::dado2(6).
1/6::dado3(1);1/6::dado3(2);1/6::dado3(3);1/6::dado3(4);1/6::dado3(5);1/6::dado3(6).
1/6::dado4(1);1/6::dado4(2);1/6::dado4(3);1/6::dado4(4);1/6::dado4(5);1/6::dado4(6).
1/6::dado5(1);1/6::dado5(2);1/6::dado5(3);1/6::dado5(4);1/6::dado5(5);1/6::dado5(6).


% yahtzee_prob(+Dados,+Patron, -Probabilidad)
yahtzee_prob(Puntaje) :-
    dado1(Dado),
    dado2(Dado),
    dado3(Dado),
    dado4(Dado),
    dado5(Dado),
    Puntaje is 50.
yahtzee_prob(Puntaje) :-
    Puntaje is 0.

%evidence(dado2(1)).
%evidence(dado1(2)).
%query(yahtzee_prob(Puntaje)).

yahtzee_prob_dados([Dado1,Dado2,Dado2,Dado4,Dado5], Prob) :- 
    subquery(yahtzee_prob(Puntaje), Prob, [dado1(Dado1), dado2(Dado2), dado3(Dado3), dado4(Dado4), dado5(Dado5)]).

%writeln(yahtzee_prob(Puntaje)).



















