# Ejecucion de Yahtzeelog

Se debe ejecutar con prolog el archivo *yahtzeelog.pl*. 
Este archivo contiene todos los predicados y tambien llama al modelo de 
problog que se encuentra en el archivo *modelo_problog_yahtzee.pl*.

Para ejecutar problog en **Windows** se debe comentar la linea 14 del archivo
*yahtzeelog.pl* y descomentar la linea 16.

Prolog escribe en el archivo *nuevo_modelo.pl* las queries para calcular
las probabilidades con evidencias.

## Estrategia Humano

Para ejecutar la estrategia humana se debe ejecutar el predicado 
yahtzeelog(humano, Seed), luego los patrones se deben ingresar con el
siguiente formato:

[0,1,1,0,1].

Donde 0 significa que no se tira el dado y 1 que se tira el dado.

La categoria se debe ingresar con el siguiente formato:

nombre_cat. 

Donde nombre_cat puede ser aces, twos, threes, fours, fives,
sixes, three_of_a_kind, four_of_a_kind, full_house, small_straight,
large_straight, yahtzee, chance.

## Estrategia ia_det

Para ejecutar la estrategia ia_det se debe ejecutar el predicado
yahtzeelog(ia_det, Seed).

## Estrategia ia_prob

Para ejecutar la estrategia ia_prob se debe ejecutar el predicado 
yahtzeelog(ia_prob, Seed).

