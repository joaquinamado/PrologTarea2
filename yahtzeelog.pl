:- use_module(library(filesex)).
:- use_module(library(random)).
:- use_module(library(readutil)).

escribir_archivo(Nombre, Contenido) :-
    open(Nombre, write, Stream),
    write(Stream, Contenido),
    close(Stream).


consultar_probabilidades(Probabilidad, Dados, Query):-
    % Problog debe estar en el path!
    % LINUX
    absolute_file_name(path(problog), Problog, [access(execute)]),
    % WINDOWS 
    %absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(modelo_problog_yahtzee,Modelo,[file_type(prolog)]),

    % Leer el contenido del modelo_problog
    read_file_to_string(Modelo, ModeloContenido, []),

    % Agrega query y dados al modelo
    term_string(Dados, DadosString),
    format(string(QueryString), 'query(~w(~w, Prob)).', [Query, DadosString]),
    atomic_list_concat([ModeloContenido, '\n', QueryString], ContenidoCompleto),

    % Crear archivo con los valores de los dados
    escribir_archivo('nuevo_modelo.pl', ContenidoCompleto),    
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    absolute_file_name(nuevo_modelo,ModeloEvidencias,[file_type(prolog)]),
    process_create(Problog,[ModeloEvidencias],[stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    split_string(Result,"\n\t","\r ",L),
    % Escribo la salida
    %writeln(Result),
    % Quito último elemento de la lista
    append(L1,[_],L),
    lista_valores(L1,Probabilidad).

% Predicado auxiliar para transformar a términos y a números, como se espera
lista_valores([X | _], Probabilidad) :-
    % Separamos la cadena usando ':'
    split_string(X, ":", "", [Cadena | _]),
    % Separamos por comas para obtener los componentes
    split_string(Cadena, ",", "", Componentes),
    % La probabilidad está en el último componente
    last(Componentes, UltimoComponente),
    % Quitamos los caracteres de cierre ')'
    split_string(UltimoComponente, ")", "", [ProbabilidadString | _]),
    % Convertimos el string a número
    number_string(Probabilidad, ProbabilidadString).
    %lista_valores(T, T1).
lista_valores([], []).

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
    listar_categorias_de_repetidos_acum(T, [aces|Acumulador], Categorias).
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

% Separa una lista que tiene solo dados repetidos en dos listas, 
% una con cada elemento distinto.
separar_repetidos([], _, []).
separar_repetidos([X,Y|Resto], [X], [Y|Resto]) :-
    X \= Y.
separar_repetidos([X,Y|Resto], [X|Resto1], Resto2) :-
    separar_repetidos([Y|Resto], Resto1, Resto2).
separar_repetidos([X], [X], []).



    
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
    (
        member(yahtzee, Juegos) -> Patron = [0,0,0,0,0]
    ; 
        (
            member(large_straight, Juegos) -> Patron = [0,0,0,0,0]
        ;
            (member(small_straight, Juegos) -> 
                sort(Dados,DadosDiferentes),
                generar_patron_diferentes(Dados, DadosDiferentes, Patron)
            ;
                ( member(full_house, Juegos) -> 
                    Patron = [1,1,1,1,1]    
                ;
                    findall(X, (nth0(_, Dados, X), contar(Dados, X, Count), Count > 1), Repetidos),
                    patron_repetidos(Dados, Repetidos, Patron)
                )
            )
        )
    ), !.
cambio_dados(Dados, _, ia_det, Patron) :-
    sort(Dados, DadosOrdenados),
    length(DadosOrdenados, Len),
    Len == 5,
    Patron = [1,1,1,1,1], !.
cambio_dados(Dados, _, ia_det, Patron) :-
    % Buscamos los dados repetidos
    findall(X, (nth0(_, Dados, X), contar(Dados, X, Count), Count > 1), Repetidos),
    patron_repetidos(Dados, Repetidos, Patron), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


cambio_dados(Dados, Tablero, ia_prob, Patron) :-
    % Busco todas las categorias con nil en Tablero
    findall(Cat, member(s(Cat, nil), Tablero), Categorias),
    % Separamos en 3 grupos, calculamos su esperanza y elegimos la mayor 
    %[small_straight, large_straight] 
    %[three_of_a_kind, four_of_a_kind, yahtzee, numero]
    %[full_house]
    % Busco las probabilidades de cada categorías
    findall(X, (nth0(_, Dados, X), contar(Dados, X, Count), Count > 1), Repetidos),
    sort(0,  @=<, Repetidos,  RepetidosOrdenados),
    separar_repetidos(RepetidosOrdenados, RepetidosGrupo1, RepetidosGrupo2),
    sort(Dados, DadosDistintos),
    (member(small_straight, Categorias) ->
        transformar_lista(DadosDistintos, DadosDistintosTransformados),
        consultar_probabilidades(ProbabilidadSml, DadosDistintosTransformados, small_straight_prob_dados),
        EsperanzaSmall is ProbabilidadSml * 30,
        ( member(large_straight, Categorias) ->
            consultar_probabilidades(ProbabilidadLrg, DadosDistintosTransformados, large_straight_prob_dados),
            EsperanzaLarge is ProbabilidadLrg * 40,
            EsperanzaEscalera is (EsperanzaSmall + EsperanzaLarge)/2
        ; 
            EsperanzaEscalera is EsperanzaSmall
        ) 
    ; 
        ( member(large_straight,Categorias) ->
        transformar_lista(DadosDistintos, DadosDistintosTransformados),
            consultar_probabilidades(ProbabilidadLrg, DadosDistintosTransformados, large_straight_prob_dados),
            EsperanzaLarge is ProbabilidadLrg * 40,
            EsperanzaEscalera is EsperanzaLarge
        ; 
            EsperanzaEscalera is 0
        )
    ),
    %write('Esperanza Escalera: '), writeln(EsperanzaEscalera),
    transformar_lista(RepetidosOrdenados, RepetidosOrdenadosTransformados),
    consultar_probabilidades(ProbabilidadFull, RepetidosOrdenadosTransformados, full_house_prob_dados),
    (member(full_house, Categorias) -> 
        EsperanzaFull is ProbabilidadFull * 25
    ; EsperanzaFull is 0
    ),
    %write('Esperanza Full: '), writeln(EsperanzaFull),
    length(RepetidosGrupo2, LenG2),
    ( LenG2 > 2 -> 
        transformar_lista(RepetidosGrupo2, RepetidosGrupo2Transformados),
        (member(yahtzee, Categorias) ->
            consultar_probabilidades(ProbabilidadYah, RepetidosGrupo2Transformados, yahtzee_prob_dados),
            EsperanzaYah is ProbabilidadYah * 50
        ;
            EsperanzaYah is 0
        ),
        (member(three_of_a_kind, Categorias) ->
            consultar_probabilidades(ProbabilidadTOK, RepetidosGrupo2Transformados, three_of_a_kind_prob_dados),
            EsperanzaTOK is ProbabilidadTOK * 20
        ; 
            EsperanzaTOK is 0
        ),
        (member(four_of_a_kind, Categorias) ->
            consultar_probabilidades(ProbabilidadFOK, RepetidosGrupo2Transformados, four_of_a_kind_prob_dados),
            EsperanzaFOK is ProbabilidadFOK * 20
        ;
            EsperanzaFOK is 0
        ),
        sort(RepetidosGrupo2, Repetidos2ListaSolo),
        sort(RepetidosGrupo2, [Elem|_]),
        listar_categorias_de_repetidos(Repetidos2ListaSolo, [Cat]),
        (member(Cat, Categorias) -> 
            EsperanzaJuegos is (EsperanzaYah + EsperanzaTOK + EsperanzaFOK + Elem*3)/3
        ;
            EsperanzaJuegos is (EsperanzaYah + EsperanzaTOK + EsperanzaFOK)/3
        )
    ; 
        transformar_lista(RepetidosGrupo1, RepetidosGrupo1Transformados),
        (member(yahtzee, Categorias) ->
            consultar_probabilidades(ProbabilidadYah, RepetidosGrupo1Transformados, yahtzee_prob_dados),
            EsperanzaYah is ProbabilidadYah * 50
        ;
            EsperanzaYah is 0
        ),
        (member(three_of_a_kind, Categorias) ->
            consultar_probabilidades(ProbabilidadTOK, RepetidosGrupo1Transformados, three_of_a_kind_prob_dados),
            EsperanzaTOK is ProbabilidadTOK * 20
        ; 
            EsperanzaTOK is 0
        ),
        (member(four_of_a_kind, Categorias) ->
            consultar_probabilidades(ProbabilidadFOK, RepetidosGrupo1Transformados, four_of_a_kind_prob_dados),
            EsperanzaFOK is ProbabilidadFOK * 20
        ;
            EsperanzaFOK is 0
        ),
        (length(RepetidosGrupo1, LenG1), LenG1 > 0 ->
            sort(RepetidosGrupo1, Repetidos1ListaSolo),
            sort(RepetidosGrupo1, [Elem1|_]),
            listar_categorias_de_repetidos(Repetidos1ListaSolo, [Cat]),
            (member(Cat, Categorias) -> 
                EsperanzaJuegos is (EsperanzaYah + EsperanzaTOK + EsperanzaFOK + Elem1*3)/3
            ;
                EsperanzaJuegos is (EsperanzaYah + EsperanzaTOK + EsperanzaFOK)/3
            )
        ;
            EsperanzaJuegos is (EsperanzaYah + EsperanzaTOK + EsperanzaFOK)/3
        )
    ),
    %write('Esperanza Juegos: '), writeln(EsperanzaJuegos),
    (EsperanzaJuegos = EsperanzaFull, EsperanzaJuegos = EsperanzaEscalera, EsperanzaJuegos = 0 -> 
        listar_categorias_de_repetidos(Dados, CategoriasDados),
        findall(CatDado, (member(CatDado, Categorias), member(CatDado, CategoriasDados)), CategoriasLibres),
        (member(CategoriaLibre, CategoriasLibres) ->
            generar_patron_de_categoria(Dados,CategoriaLibre,Patron)
        ; 
            Patron = [1,1,1,1,1]
        )
    ;
        (EsperanzaEscalera > EsperanzaFull -> 
            (EsperanzaEscalera > EsperanzaJuegos -> 
                %HACER PATRON CON DADOS DIFERENTES
                generar_patron_diferentes(Dados, DadosDistintos, Patron)
            ; 
                (EsperanzaJuegos > EsperanzaFull -> 
                    %HACER PATRON CON DADOS REPETIDOS (GRUPO MAYOR)
                    (LenG2 > 2 -> 
                        generar_patron(Dados, RepetidosGrupo2, Patron)
                    ;
                        generar_patron(Dados, RepetidosGrupo1, Patron)
                    )

                ;   
                %HACER PATRON CON DADOS REPETIDOS (TODOS PARA FULL)
                generar_patron(Dados, RepetidosOrdenados, Patron)
                )
            )
        ;
            (EsperanzaFull > EsperanzaJuegos -> 
                    %HACER PATRON CON DADOS REPETIDOS (TODOS PARA FULL)
                    generar_patron(Dados, RepetidosOrdenados, Patron)
            ; 
                    %HACER PATRON CON DADOS REPETIDOS (GRUPO MAYOR)
                    (LenG2 > 2 -> 
                        generar_patron(Dados, RepetidosGrupo2, Patron)
                    ;
                        generar_patron(Dados, RepetidosGrupo1, Patron)
                    )
            )
        )
    ), !.

%Transforma una lista de dados [1,2,3,4,5] en una 
%lista de términos [dado1(1),dado2(2),dado3(3),dado4(4),dado5(5)]
transformar_lista(Dados, DadosTransformados) :-
    transformar_lista_aux(Dados, 1, DadosTransformados).

transformar_lista_aux([], _, []).

transformar_lista_aux([D|R], N, [TermAtom|TR]) :-
    format(atom(Term), 'dado~w(~w)', [N, D]),
    atom_to_term(Term, TermAtom, []),
    N1 is N + 1,
    transformar_lista_aux(R, N1, TR).

% Genero patron para los casos sin dados repetidos 
% [aces, twos, threes, fours, fives, sixes]
generar_patron_de_categoria(Dados, Categoria, Patron) :-
    valor_categoria(Categoria, Valor),
    generar_patron_de_categoria_aux(Dados, Valor, Patron).

valor_categoria(aces, 1).
valor_categoria(twos, 2).
valor_categoria(threes, 3).
valor_categoria(fours, 4).
valor_categoria(fives, 5).
valor_categoria(sixes, 6).

generar_patron_de_categoria_aux([], _, []).

generar_patron_de_categoria_aux([D|R], Valor, [P|PR]) :-
    ( D =:= Valor -> P = 0 ; P = 1 ),
    generar_patron_de_categoria_aux(R, Valor, PR).

% Genero patron para los casos con dados repetidos 
% [yahtzee, full_house, three_of_a_kind, four_of_a_kind]
generar_patron(Dados, DadosPatron, Patron) :-
    generar_patron_aux(Dados, DadosPatron, Patron).

generar_patron_aux([], _, []).

generar_patron_aux([D|R], DadosPatron, [P|PR]) :-
    ( member(D, DadosPatron) -> P = 0 ; P = 1 ),
    generar_patron_aux(R, DadosPatron, PR).

% Genero el patron para los casos con dados diferentes
% [small_straight, large_straight]
generar_patron_diferentes(Dados, DadosDiferentes, Patron) :-
    generar_patron_diferente_aux(Dados, DadosDiferentes, [], Patron).

generar_patron_diferente_aux([], _,_, []).

generar_patron_diferente_aux([D|R], DadosDiferentes, Usados, [P|PR]) :-
    ( memberchk(D, DadosDiferentes), \+ memberchk(D, Usados) ->
        P = 0,
        generar_patron_diferente_aux(R, DadosDiferentes, [D|Usados], PR)
    ; 
        P = 1,
        generar_patron_diferente_aux(R, DadosDiferentes, Usados, PR)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%eleccion_slot(+Dados,+Tablero,+Estrategia,-Categoria)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   HUMANO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eleccion_slot(Dados, Tablero, humano, Categoria) :-
    findall(Cat, member(s(Cat, nil), Tablero), Categorias),
    write('Dados: '), nl,
    write(Dados), nl,
    write('Ingrese la categoría en la que desea anotar los puntos:'), nl,
    write('Categorías disponibles: '), writeln(Categorias),
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
    member(Categoria, [aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  IA_PROB 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
    es_juego(Dados, Tablero, Juegos),
    Juegos \= [],
    elegir_mejor_juego(Categoria, Juegos), !.
eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
    % Segundo caso de elección: si hay dados repetidos, elegir la categoría de los dados repetidos 
    findall(X, (member(X, Dados), contar(Dados, X, Count), Count > 1), RepetidosDuplicados),
    sort(RepetidosDuplicados, Repetidos),
    Repetidos \= [],
    elegir_categoria_repetida(Tablero, Categoria, Repetidos), !.
eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
    % Tercer caso de elección: si no hay dados repetidos, elegir la categoría con menor valor
    ( member(s(chance, nil), Tablero) -> Categoria = chance
    ; elegir_categoria_menor_valor(Dados,Tablero, Categoria) ), !.
eleccion_slot(_, Tablero, ia_prob, Categoria) :-
    % Cuarto caso de elección: si no se cumple ninguno de los casos anteriores, 
    % elegir la categoria de menor valor
    member(s(Categoria, nil), Tablero),
    member(Categoria, [aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]), !.

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
    (Patron = [0,0,0,0,0] -> 
        eleccion_slot(Dados,Tablero,ia_prob,Slot),
        write('Slot elegido:'),writeln(Slot),
        puntaje(Dados,Slot,Punt)
    ;
        lanzamiento(Dados,Patron,Dados1),
        write('Segundo Lanzamiento:'),writeln(Dados1),
        cambio_dados(Dados1,Tablero,ia_prob,Patron1),
        write('Patron sugerido:'),writeln(Patron1),
        (Patron1 = [0,0,0,0,0] -> 
            eleccion_slot(Dados1,Tablero,ia_prob,Slot),
            write('Slot elegido:'),writeln(Slot),
            puntaje(Dados1,Slot,Punt)
        ;
            lanzamiento(Dados1,Patron1,Dados2),
            write('Tercer Lanzamiento:'),writeln(Dados2),
            eleccion_slot(Dados2,Tablero,ia_prob,Slot),
            write('Slot elegido:'),writeln(Slot),
            puntaje(Dados2,Slot,Punt)
        )
    ),
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
