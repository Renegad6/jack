/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        write("Sombras sobre Londres... v.0.1, (C) Jorge de Antonio,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        random(1,200,G),
        assertz(guarida(G))
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl.

otra_noche_mas :-
        jack_libre,
        queda_por_matar,
        noche(N),retract(noche(_)),N is N+1,assertz(noche(N)).
        retract(pe(_,_)),
        cuantas_pes(N,P),
        coloca_pes_una_a_una(P).
        retract(linternas_que_quedan(_)),linternas_por_noche(N,LI),assertz(linternas_que_quedan(LI)),
        retract(carromatos_que_quedan(_)),carromatos_por_noche(N,CARR),assertz(carromatos_que_quedan(CARR)),
        write(" .... ya he colocado las pe's, comienza noche "),write(N),write(",bwahahahaha...."),nl.

mata_una :-
        jack_mata_una(C),
        write(" .... Jack mata en.. "),write(C),write(", ... bwahahahaha"),nl,
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        jack_en(C).

mata_dos :-
        jack_mata_una(C),
        jack_mata_una(CC),
        write(" .... Jack mata en.. "),write(C),write(", y"),write(CC),write(", ... bwahahahaha"),nl,
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        elige_donde_jack(C,CC,CR),
        jack_en(CR).

mueve_jack :-
        jack_libre,
        queda_por_matar,
        jack_no_en_guarida,
        jack_en(C),
        guarida(G),
        hay_caminos(C,G,L),
        mejor_camino(L,posicion_del_mapa(CI,CD,LI,CARR),
        assertz(jack_ha_estado(C)),
        assertz(jack_ha_estado(CI)),
        anuncia_movimiento(LI,CARR),
        avisa_si_en_guarida.

donde_poli(P) :-
        jack_libre,
        queda_por_matar,
        write("Poli:"),write(P),nl,
        write("  Entre:"),read(A),nl,
        write("  y:"),read(B),nl,
        poli_esta_en(P,A,B).

arresto(C) :-
        jack_libre,
        queda_por_matar,
        jack_en(C),
        write("... aaarggghhhh me habeis pilladooooooooooooooooo......"),
        banner,
        retract(jack_libre).

pista(C) :-
        jack_libre,
        queda_por_matar,
        jack_ha_estado(C),
        write(".... ssiiii!!!!!"),nl.
pista(_) :- write(" .... mmmmmmmhhh  no!! :D"),nl.


/* Funciones auxiliares .*/
init:-
        retract(guarida(_)),
        retract(poli(_,_,_)),
        retract(crime_scene(_)),
        assertz(jack_libre),
        assertz(queda_por_matar),
        retract(noche(_)),
        retract(jack_ha_estado(_)),
        assertz(noche(0)).

coloca_pes_una_a_una (P) :-
        (P > 0),
        coloca_pe,
        coloca_pes_una_a_una(P-1).
coloca_pes_una_a_una(0).

coloca_pe :-
        random(1,7,P),
        pe_no_ha_sido_colocada(P),
        salida_pe(P,C),
        not_crime_scene(C),
        assertz(pe(P,C)).
        
not_crime_scene(C) :- 
        crime_scene(C),fail.
not_crime_scene(C).

pe_no_ha_sido_colocada(P) :-
        pe(P,_),fail.
pe_no_ha_sido_colocada(P).

jack_mata_una(C) :-
        random(1,7,P),
        pe(P,C),retract(pe(P,C)),

jack_en(C) :-
        retract(posicion_jack(_)),assertz(posicion_jack(C)).

elige_donde_jack(C,CC,RC) :-
        random(0,1,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

jack_no_en_guarida :- posicion_jack(C),guarida(G),C \== G.
jack_en_guarida :- posicion_jack(C),guarida(C).

poli_esta_en(P,0,0):-.
poli_esta_en(P,A,B):-
        assertz(poli(P,A,B)),
        donde_poli(P).
        
avisa_si_en_guarida :-
        jack_en_guarida,write("Llegue a mi guarida, bwaahhahahahaaha"),nl.
avisa_si_en_guarida :-.

anuncia_movimiento(1,_):-
        write(".. voy a usar ... una linterna!! me meti por una callejon!! bwahahahaha"),nl.
anuncia_movimiento(_,1):-
        write(".. voy a usar ... un carromato!! voy a toda pastillaao!! bwahahahaha"),nl.
banner:-
        write("                             ud$$$**$$$$$$$bc.                          "),nl,
        write("                          u@**%        4$$$$$$$Nu                       "),nl,
        write("                        J                %%#$$$$$$r                     "),nl,
        write("                       @                       $$$$b                    "),nl,
        write("                     .F                        ^*3$$$                   "),nl,
        write("                    :% 4                         J$$$N                  "),nl,
        write("                    $  :F                       :$$$$$                  "),nl,
        write("                   4F  9                       J$$$$$$$                 "),nl,
        write("                   4$   k             4$$$$bed$$$$$$$$$                 "),nl,
        write("                   $$r  %F            $$$$$$$$$$$$$$$$$r                "),nl,
        write("                   $$$   b.           $$$$$$$$$$$$$$$$$N                "),nl,
        write("                   $$$$$k 3eeed$$b    $$$Euec.%$$$$$$$$$                "),nl,
        write("    .@$**N.        $$$$$% $$$$$$F%L $$$$$$$$$$$  $$$$$$$                "),nl,
        write("    :$$L  %L       $$$$$ 4$$$$$$  * $$$$$$$$$$F  $$$$$$F         edNc   "),nl,
        write("   @$$$$N  ^k      $$$$$  3$$$$*%   $F4$$$$$$$   $$$$$%        d%  z$N  "),nl,
        write("   $$$$$$   ^k     %$$$%   #$$$F   .$  $$$$$c.u@$$$          J%  @$$$$r "),nl,
        write("   $$$$$$$b   *u    ^$L            $$  $$$$$$$$$$$$u@       $$  d$$$$$$ "),nl,
        write("    ^$$$$$$.    %NL   %N. z@*     $$$  $$$$$$$$$$$$$P      $P  d$$$$$$$ "),nl,
        write("       ^%*$$$$b   %*L   9$E      4$$$  d$$$$$$$$$$$%     d*   J$$$$$r   "),nl,
        write("            ^$$$$u  %$.  $$$L     %#% d$$$$$$%.@$$    .@$%  z$$$$*%     "),nl,
        write("              ^$$$$. ^$N.3$$$       4u$$$$$$$ 4$$$  u$*% z$$$%          "),nl,
        write("                %*$$$$$$$$ *$b      J$$$$$$$b u$$P $%  d$$P             "),nl,
        write("                   #$$$$$$ 4$ 3*$%$*$ $%$%c@@$$$$ .u@$$$P               "),nl,
        write("                     %$$$$  %%F~$ $uNr$$$^&J$$$$F $$$$#                 "),nl,
        write("                       %$$    %$$$bd$.$W$$$$$$$$F $$%                   "),nl,
        write("                         ?k         ?$$$$$$$$$$$F%*                     "),nl,
        write("                          9$$bL     z$$$$$$$$$$$F                       "),nl,
        write("                           $$$$    $$$$$$$$$$$$$                        "),nl,
        write("                            %#$$c  %$$$$$$$$$%                          "),nl,
        write("                             .@%#$$$$$$$$$$$$b                          "),nl,
        write("                           z*      $$$$$$$$$$$$N.                       "),nl,
        write("                         e%      z$$%  #$$$k  %*$$.                     "),nl,
        write("                     .u*      u@$P%      %#$$c   %$$c                   "),nl,
        write("              u@$*%%%       d$$%            %$$$u  ^*$$b.               "),nl,
        write("            :$F           J$P%                ^$$$c   %%$$$$$$bL        "),nl,
        write("           d$$  ..      @$#                      #$$b         %#$       "),nl,
        write("           9$$$$$$b   4$$                          ^$$k         %$      "),nl,
        write("            %$$6%%$b u$$                             %$    d$$$$$P      "),nl,
        write("              %$F $$$$$%                              ^b  ^$$$$b$       "),nl,
        write("               %$W$$$$%                                %b@$$$$%         "),nl,
        write("                                                        ^$$$*           "),nl.

/* movimiento */

hay_caminos(P,G,LI,CA,MEJ_CAM):-
        camino(mapa(P,_,15,LI,CA),mapa(G,_,_,_,_),CAM),
        elige_mejor_camino(MEJ_CAM,CAM,MEJ_CAM2),
        (MEJ_CAM is MEJ_CAM2).

elige_mejor_camino(C1,C2,C3):-
        length(C1,L1),
        length(C2,L2),
        (L1=<L2),
        (C3 is C1).
elige_mejor_camino(C1,C2,C3):-
        length(C1,L1),
        length(C2,L2),
        (L1>L2),
        (C3 is C2).

camino(mapa(A,_,M,LI,CA),mapa(B,A,M2,LI,CA),[mapa(A,_,M,LI,CA),mapa(B,A,M2,LI,CA)]):-
        (M>0),jack_camina(A,B),(M2 is (M-1)).
camino(mapa(A,_,M,LI,CA),mapa(B,A,M2,LI2,CA),[mapa(A,_,M,LI,CA),mapa(B,A,M2,LI2,CA)]):-
        (M>0),jack_pasa_por_callejon(A,B),(M2 is (M-1)),(LI2 is (LI-1))
camino(mapa(A,_,M,LI,CA),mapa(C,B,M2,LI,CA2),[mapa(A,_,M,LI,CA),mapa(C,B,M2,LI,CA2)]):-
        (M>1),jack_va_en_carromato(A,B,C),(M2 is (M-2)),(CA2 is (CA-1))

camino(mapa(A,M,L,C),mapa(B,M2,L,C),[mapa(A,M,L,C)|RESTO]):-
        (M>0),jack_camina(A,W),(M2 is (M-1)),
        camino(mapa(W,M2,L,C),mapa(B,_,_,_),RESTO).

jack_camina(A,B):-
        conectados(A,B),
        no_hay_polis(A,B).
jack_pasa_por_callejon(A,B):-
        callejon(A,B).
jack_pasa_por_callejon(A,B):-
        callejon(B,A).
jack_va_en_carromato(A,B,C):-
        conectados(A,B),no_hay_polis(A,B).
        conectados(B,C),no_hay_polis(B,C).

conectados(A,B):-
        conexion(A,B).
conectados(A,B):-
        conexion(B,A).

no_hay_polis(A,B):-
        poli(_,A,B),fail.
no_hay_polis(A,B):-
        poli(_,B,A),fail.
no_hay_polis(_,_):-.

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
