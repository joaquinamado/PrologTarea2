:- use_module(library(filesex)).
:- use_module(library(random)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).

% Lanza un dado
tiro_dado(X):-
    random(1,7,X).


%tablero_test([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
%    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0), 
%    s(large_straight,40), s(yahtzee,50),s(chance,10)]).

% Inicializo el tablero a partir de la lista de categorías
tablero_inicial([],[]).
tablero_inicial([Cat|Cats],[s(Cat,nil)|T1]):-
        tablero_inicial(Cats,T1).


% Contar cuántas veces aparece un número en una lista
contar([],_,0).
contar([X|T],Y,N):-
    X \= Y,
    contar(T,Y,N).
contar([X|T],Y,N):-
    X = Y,
    contar(T,Y,N1),
    N is N1+1.

three_of_a_kind(List, true, X) :-
    member(X, [1, 2, 3, 4, 5, 6]),
    contar(List, X, Count),
    Count >= 3, !.

three_of_a_kind(_, false, _).

four_of_a_kind(List, true) :-
    member(X, [1, 2, 3, 4, 5, 6]),
    contar(List, X, Count),
    Count >= 4, !.

four_of_a_kind(_, false).

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is TailSum + H.

is_full_house(List, true) :-
    member(X, [1, 2, 3, 4, 5, 6]),
    contar(List, X, Count),
    Count = 3,
    member(Y, [1, 2, 3, 4, 5, 6]),
    contar(List, Y, Count2),
    Count2 = 2,
    X \= Y, !.

is_full_house(_, false).

small_straight(List, true) :-
    member(1, List),
    member(2, List),
    member(3, List),
    member(4, List).
small_straight(List, true) :-
    member(2, List),
    member(3, List),
    member(4, List),
    member(5, List).
small_straight(List, true) :-
    member(3, List),
    member(4, List),
    member(5, List),
    member(6, List).
small_straight(_, false).

large_straight(List, true) :-
    member(1, List),
    member(2, List),
    member(3, List),
    member(4, List),
    member(5, List).
large_straight(List, true) :-
    member(2, List),
    member(3, List),
    member(4, List),
    member(5, List),
    member(6, List).
large_straight(_, false).


yahtzee(List, [X|_], 5) :-
    member(X, List),
    contar(List, X, 5), !.

puntaje_tablero_aux([], 0, 0).
puntaje_tablero_aux([s(Cat,P)|T], PuntajeNums, PuntajeJuegos):-
    member(Cat, [aces,twos,threes,fours,fives,sixes]),
    puntaje_tablero_aux(T, PuntajeNums1, PuntajeJuegos),
    PuntajeNums is PuntajeNums1 + P.

puntaje_tablero_aux([s(Cat,P)|T], PuntajeNums, PuntajeJuegos):-
    member(Cat, [three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]),
    puntaje_tablero_aux(T, PuntajeNums, PuntajeJuegos1),
    PuntajeJuegos is PuntajeJuegos1 + P.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% puntaje(+Dados, +Cat, -Puntos)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

puntaje(Dados, aces, Puntos):-
    contar(Dados, 1, Puntos).
puntaje(Dados, twos, Puntos):-
    contar(Dados, 2, Elems),
    Puntos is Elems * 2.
puntaje(Dados, threes, Puntos):-
    contar(Dados, 3, Elems),
    Puntos is Elems * 3.
puntaje(Dados, fours, Puntos):-
    contar(Dados, 4, Elems),
    Puntos is Elems * 4.
puntaje(Dados, fives, Puntos):-
    contar(Dados, 5, Elems),
    Puntos is Elems * 5.
puntaje(Dados, sixes, Puntos):-
    contar(Dados, 6, Elems),
    Puntos is Elems * 6.
puntaje(Dados, three_of_a_kind, Puntos):-
    three_of_a_kind(Dados, true, _),
    sum_list(Dados, Puntos), !.
puntaje(Dados, three_of_a_kind, Puntos):-
    three_of_a_kind(Dados, false, _),
    Puntos is 0.
puntaje(Dados, four_of_a_kind, Puntos):-
    four_of_a_kind(Dados, true),
    sum_list(Dados, Puntos), !.
puntaje(Dados, four_of_a_kind, Puntos):-
    four_of_a_kind(Dados, false),
    Puntos is 0.
puntaje(Dados, full_house, Puntos):-
    is_full_house(Dados, true),
    Puntos is 25, !.
puntaje(Dados, full_house, Puntos):-
    is_full_house(Dados, false),
    Puntos is 0.
puntaje(Dados, small_straight, Puntos):-
    small_straight(Dados, true),
    Puntos is 30, !.
puntaje(Dados, small_straight, Puntos):-
    small_straight(Dados, false),
    Puntos is 0.
puntaje(Dados, large_straight, Puntos):-
    large_straight(Dados, true),
    Puntos is 40, !.
puntaje(Dados, large_straight, Puntos):-
    large_straight(Dados, false),
    Puntos is 0.
puntaje(Dados, yahtzee, Puntos):-
    yahtzee(Dados, Dados, 5),
    Puntos is 50, !.
puntaje(_, yahtzee, Puntos):-
    Puntos is 0.
puntaje(Dados, chance, Puntos):-
    sum_list(Dados, Puntos).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% puntaje_tablero(+Tablero, -Puntaje)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

puntaje_tablero(Tablero, Puntaje) :-
    puntaje_tablero_aux(Tablero, PuntajeNums, PuntajeJuegos),
    PuntajeNums >= 63,
    Puntaje is PuntajeNums + PuntajeJuegos + 35, !.
puntaje_tablero(Tablero, Puntaje) :-
    puntaje_tablero_aux(Tablero, PuntajeNums, PuntajeJuegos),
    PuntajeNums < 63,
    Puntaje is PuntajeNums + PuntajeJuegos, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ajustar_tablero(+Tablero,+Categoria,+Puntaje,-TableroSalida)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ajustar_tablero([],_,_,[]).
ajustar_tablero([s(Cat,_)|T],Cat,Puntaje,[s(Cat,Puntaje)|T]) :- !.
ajustar_tablero([H|T], Categoria, Puntaje, [H|T2]) :-
    H = s(Cat, _),
    Cat \= Categoria,
    ajustar_tablero(T, Categoria, Puntaje, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cambio_dados(+Dados, +Tablero, +Estrategia, -Patron)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   HUMANO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambio_dados(Dados, _, humano, Patron) :-
    write('Ingrese los dados que desea mantener (0 para mantener, 1 para lanzar de nuevo):'), nl,
    write(Dados), nl,
    read(Patron).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_DET 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambio_dados(Dados, _, ia_det, Patron).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambio_dados(Dados, _, ia_prob, Patron).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%eleccion_slot(+Dados,+Tablero,+Estrategia,-Categoria)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   HUMANO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eleccion_slot(Dados, Tablero, humano, Categoria) :-
    write('Dados: '), nl,
    write(Dados), nl,
    write('Ingrese la categoría en la que desea anotar los puntos:'), nl,
    read(Categoria),
    % Verificar que la categoría sea válida
    member(Categoria, [aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]),
    % Verificar que la categoría no haya sido elegida
    member(s(Categoria,nil), Tablero), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_DET 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eleccion_slot(Dados, Tablero, ia_det, Categoria).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eleccion_slot(Dados, Tablero, ia_prob, Categoria).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 YAHTZEELOG 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Jugador yahtzee
% Jugador puede ser humano o ia
yahtzeelog(Estrategia,Seed):-
    set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  HUMANO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partida(humano,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,humano,Tablero,TableroFinal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partida(ia_prob,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,ia_prob,Tablero,TableroFinal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_DET 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partida(ia_det,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,ia_det,Tablero,TableroFinal).


% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  HUMANO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ronda(NumRonda,humano,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,humano,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,humano,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,humano,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,humano,Tablero2,TableroSalida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ronda(NumRonda,ia_prob,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,ia_prob,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,ia_prob,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,ia_prob,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,ia_prob,Tablero2,TableroSalida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_DET 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ronda(NumRonda,ia_det,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,ia_det,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,ia_det,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,ia_det,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,ia_det,Tablero2,TableroSalida).
