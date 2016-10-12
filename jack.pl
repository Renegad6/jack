/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        write("Sombras sobre Londres... v.0.1, (C) Jorge de Antonio,"),nl,
        init,
        elige_guarida,
        noche_normal,
        noche_normal
        noche_especial,
        noche_normal,
        fin.

init:-
        retract(guarida(_)),
        retract(poli(_,_,_)),
        retract(crime_scene(_)),
        assert(jack_libre),
        retract(noche(_)),
        assert(noche(0)).

elige_guarida :-
        random(1,200,G),
        assert(guarida(G)).

noche_normal :-
        otra_noche_mas
        jack_libre,
        coloca_pes,
        jack_mata_una(C),
        jack_en(C),
        rondas_empieza_jack.

noche_especial :-
        jack_libre,
        coloca_pes,
        jack_mata_una(C),
        jack_mata_una(CC),
        elige_donde_jack(C,CC,RC),
        jack_en(RC),
        rondas_empiezan_polis.

coloca_pes :-
        retract(pe(_,_)),
        noche(N),
        cuantas_pes(N,P),
        coloca_pes_una_a_una(T,P).

coloca_pes_una_a_una (P) :-
        P > 0,
        coloca_pe,
        coloca_pes_una_a_una(P-1).
coloca_pes_una_a_una(0).

coloca_pe :-
        random(1,7,P),
        pe_no_ha_sido_colocada(P),
        salida_pe(P,C),
        not_crime_scene(C),
        assert(pe(P,C)).
        
not_crime_scene(C) :- 
        crime_scene(C),fail.
not_crime_scene(C).

pe_no_ha_sido_colocada(P) :-
        pe(P,_),fail.
pe_no_ha_sido_colocada(P).

otra_noche_mas :- 
        noche(X),retract(noche(_)),assert(noche(X+1)).

jack_mata_una(C) :-
        random(1,7,P),
        pe(P,C),retract(pe(P,C)),
        write(" .... Jack mata en.. "),write(C),nl.

jack_en(C) :-
        retract(posicion_jack(_)),assert(posicion_jack(C)).

elige_donde_jack(C,CC,RC) :-
        random(0,1,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

rondas_empieza_jack :-
        jack_no_en_guarida,
        jack_libre,
        mueve_jack,
        mueven_polis,
        rondas_empieza_jack.
rondas_empieza_jack:-.

rondas_empiezan_polis :-
        jack_no_en_guarida,
        jack_libre,
        mueven_polis,
        mueve_jack,
        rondas_empiezan_polis.
rondas_empiezan_polis:-.

jack_no_en_guarida :- posicion_jack(C),guarida(G),C \== G.

mueven_polis :-
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        retract(poli(_,_,_),
        donde_poli(rojo),
        donde_poli(azul),
        donde_poli(amarillo),
        donde_poli(verde),
        donde_poli(marron).

donde_poli(P) :-
        write("Poli:"),write(P),nl,
        write("  Entre:"),read(A),nl,
        write("  y:"),read(B),nl,
        poli_esta_en(P,A,B).

poli_esta_en(P,0,0):-.
poli_esta_en(P,A,B):-
        assert(poli(P,A,B)),
        donde_poli(P).
        

/* Configuracion del juego */

/* prostituas  por noche */
cuantas_pes(1,5).
cuantas_pes(2,4).
cuantas_pes(3,3).
cuantas_pes(4,2).

/* posiciones de salida de las pes */
salida_pe(1,134).
salida_pe(2,134).
salida_pe(3,134).
salida_pe(4,134).
salida_pe(5,134).
salida_pe(7,134).
