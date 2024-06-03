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

% es_juego(+Dados, +Tablero, -Juego)
es_juego(Dados, Tablero, Juego) :-
    findall(Cat, juego_categoria(Dados, Tablero, Cat), Juego).

juego_categoria(Dados, Tablero, three_of_a_kind) :-
    member(s(three_of_a_kind, nil), Tablero),
    three_of_a_kind(Dados, true, _).
juego_categoria(Dados, Tablero, four_of_a_kind) :-
    member(s(four_of_a_kind, nil), Tablero),
    four_of_a_kind(Dados, true).
juego_categoria(Dados, Tablero, full_house) :-
    member(s(full_house, nil), Tablero),
    is_full_house(Dados, true).
juego_categoria(Dados, Tablero, small_straight) :-
    member(s(small_straight, nil), Tablero),
    small_straight(Dados, true).
juego_categoria(Dados, Tablero, large_straight) :-
    member(s(large_straight, nil), Tablero),
    large_straight(Dados, true).
juego_categoria(Dados, Tablero, yahtzee) :-
    member(s(yahtzee, nil), Tablero),
    yahtzee(Dados, Dados, 5).


% Crear patron en base a los dados repetidos
patron_repetidos([], _, []).
patron_repetidos([H|T], Repetidos, [P|Patron]) :-
    ( member(H, Repetidos) -> P = 0 ; P = 1 ),
    patron_repetidos(T, Repetidos, Patron).

% elige la mejor categoría para anotar los puntos
% elegir_mejor_juego(-Cat, +Juegos)
elegir_mejor_juego(Cat, Juegos) :-
    ListaJuegos = [yahtzee, large_straight, small_straight, full_house, four_of_a_kind, three_of_a_kind, chance],
    member(Cat, ListaJuegos),
    member(Cat, Juegos), !.

% Dada una lista con valores de dados no repetidos, 
% asigna a Categorias las categorias de esos dados.
% listar_categorias_de_repetidos(+Lista, -Categorias)
listar_categorias_de_repetidos(Lista, Categorias) :-
    listar_categorias_de_repetidos_acum(Lista, [], Categorias).

listar_categorias_de_repetidos_acum([], Acumulador, Acumulador).
listar_categorias_de_repetidos_acum([1|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [ones|Acumulador], Categorias).
listar_categorias_de_repetidos_acum([2|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [twos|Acumulador], Categorias).
listar_categorias_de_repetidos_acum([3|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [threes|Acumulador], Categorias).
listar_categorias_de_repetidos_acum([4|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [fours|Acumulador], Categorias).
listar_categorias_de_repetidos_acum([5|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [fives|Acumulador], Categorias).
listar_categorias_de_repetidos_acum([6|T], Acumulador, Categorias) :-
    listar_categorias_de_repetidos_acum(T, [sixes|Acumulador], Categorias).

% Elige la categoria para los dados repetidos en el tablero
elegir_categoria_repetida(Tablero, Categoria, Repetidos) :-
    listar_categorias_de_repetidos(Repetidos, CategoriasRepetidas),
    findall(Cat, (member(s(Cat, nil), Tablero), 
        member(Cat, [aces, twos, threes, fours, fives, sixes]),
        member(Cat, CategoriasRepetidas)),
        CategoriasPosibles),
        write('Categorias Posibles: '), writeln(CategoriasPosibles),
        ( CategoriasPosibles \= [] -> member(Categoria, CategoriasPosibles) 
        ; (member(s(chance, nil), Tablero) -> Categoria = chance ) 
        ; false), !.

elegir_categoria_menor_valor(Dados, Tablero, Categoria) :-
    listar_categorias_de_repetidos(Dados, CategoriasRepetidas),
    findall(Cat, 
        (member(s(Cat, P), Tablero), 
         member(Cat, [aces, twos, threes, fours, fives, sixes]), 
         member(Cat, CategoriasRepetidas),
         P = nil), 
        PuntajesCategorias),  
    PuntajesCategorias \= [],
    member(Categoria, PuntajesCategorias), !.



    
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

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    es_juego(Dados,Tablero, Juegos),
    Juegos \= [],
    Patron = [0,0,0,0,0], !.
cambio_dados(Dados, _, ia_det, Patron) :-
    sort(Dados, DadosOrdenados),
    length(DadosOrdenados, Len),
    Len == 5,
    Patron = [1,1,1,1,1], !.
cambio_dados(Dados, Tablero, ia_det, Patron) :-
    % Buscamos los dados repetidos
    findall(X, (nth0(Index, Dados, X), contar(Dados, X, Count), Count > 1), Repetidos),
    patron_repetidos(Dados, Repetidos, Patron), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambio_dados(Dados, Tablero, ia_prob, Patron).


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

eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
    % Primer caso de elección: si hay un juego posible, elegir el mejor
    es_juego(Dados, Tablero, Juegos),
    Juegos \= [],
    elegir_mejor_juego(Categoria, Juegos), !.
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
    % Segundo caso de elección: si hay dados repetidos, elegir la categoría de los dados repetidos 
    findall(X, (member(X, Dados), contar(Dados, X, Count), Count > 1), RepetidosDuplicados),
    sort(RepetidosDuplicados, Repetidos),
    Repetidos \= [],
    elegir_categoria_repetida(Tablero, Categoria, Repetidos), !.
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
    % Tercer caso de elección: si no hay dados repetidos, elegir la categoría con menor valor
    ( member(s(chance, nil), Tablero) -> Categoria = chance
    ; elegir_categoria_menor_valor(Dados,Tablero, Categoria) ), !.
eleccion_slot(_, Tablero, ia_det, Categoria) :-
    % Cuarto caso de elección: si no se cumple ninguno de los casos anteriores, 
    % elegir la categoria de menor valor
    member(s(Categoria, nil), Tablero),
    member(Categorias, [aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]), !.


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
