/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        banner,
        write("Sombras sobre Londres... v.0.1, (C) Jorge de Antonio,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        random(1,4,G),
        abolish(guarida,1),assertz(guarida(G)),
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl.

otra_noche_mas :-
        jack_libre(yes),
        queda_por_matar(yes),
        noche(N),retract(noche(_)),(N2 is N+1),assertz(noche(N2)),
        abolish(pe,2),assert(pe(1,0)),assert(pe(2,0)),assert(pe(3,0)),assert(pe(4,0)),assert(pe(5,0)),assert(pe(6,0)),assert(pe(7,0)),
        cuantas_pes(N2,P),
        coloca_pes_una_a_una(P),
        coloca_polis,
        abolish(linternas_que_quedan,1),linternas_por_noche(N2,LI),assertz(linternas_que_quedan(LI)),
        abolish(carromatos_que_quedan,1),carromatos_por_noche(N2,CARR),assertz(carromatos_que_quedan(CARR)),
        abolish(movimientos_que_quedan,1),assertz(movimientos_que_quedan(15)),
        abolish(posicion_jack,1),abolish(jack_ha_estado,1),assertz(posicion_jack(0)),assertz(jack_ha_estado(0)),
        write(" .... ya he colocado las pe's, comienza noche "),write(N2),write(",bwahahahaha...."),nl.

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
        jack_libre(yes),
        queda_por_matar(yes),
        \+ jack_en_guarida,
        posicion_jack(P),
        guarida(G),
        movimientos_que_quedan(M),
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        camino(P,G,M,LI,CA,[],CAM),!,
        procesa_etapa(CAM).

donde_poli(P) :-
        jack_libre(yes),
        queda_por_matar(yes),
        write("Poli:"),write(P),nl,
        write("  Entre:"),read(A),nl,
        write("  y:"),read(B),nl,
        poli_esta_en(P,A,B).

arresto(C):-
        jack_libre(yes),
        queda_por_matar(yes),
        posicion_jack(C),
        write("... aaarggghhhh me habeis pilladooooooooooooooooo......"),
        retract(jack_libre(yes)),
        assertz(jack_libre(no)).

pista(C) :-
        jack_libre(yes),
        queda_por_matar(yes),
        jack_ha_estado(C),
        write(".... ssiiii!!!!!"),nl.
pista(_) :- write(" .... mmmmmmmhhh  no!! :D"),nl.


/* Funciones auxiliares .*/
init :-
        abolish(guarida,1),
        abolish(poli,3),
        abolish(crime_scene,1),assertz(crime_scene(0)),
        abolish(jack_libre,1),assertz(jack_libre(yes)),
        abolish(posicion_jack,1),
        abolish(queda_por_matar,1),assertz(queda_por_matar(yes)),
        abolish(noche,1),
        abolish(jack_ha_estado,1),
        assertz(noche(0)).

coloca_pes_una_a_una(0).
coloca_pes_una_a_una(P) :-
        P>0,
        coloca_pe(P),
        P2 is P-1,
        coloca_pes_una_a_una(P2).
coloca_pe(P):-
        salida_pe(P,C),
        \+crime_scene(C),
        retract(pe(P,_)),assertz(pe(P,C)).
coloca_polis:-
        assertz(poli(rojo,2,3)).
        
jack_mata_una(C) :-
        random(1,7,P),
        pe(P,C).

jack_en(C) :-
        abolish(posicion_jack,1),assertz(posicion_jack(C)).

elige_donde_jack(C,CC,RC) :-
        random(0,1,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

jack_en_guarida :- posicion_jack(C),guarida(C).

poli_esta_en(_,0,0).
poli_esta_en(P,A,B):-
        assertz(poli(P,A,B)),
        donde_poli(P).
        
avisa_si_en_guarida :-
        jack_en_guarida,write("Llegue a mi guarida, bwaahhahahahaaha"),nl.
avisa_si_en_guarida.

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
        camino(etapa(P,_,15,LI,CA),etapa(G,_,_,_,_),_,CAM),
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

camino(A,B,M,_,_,[A],[etapa(B,no,no)]):-
        M>0,jack_camina(A,B).
camino(A,B,M,_,_,[A],[etapa(B,yes,no)]):-
        M>0,jack_pasa_por_callejon(A,B).
camino(A,B,M,_,_,_,[etapa(W,no,yes),etapa(B,no,yes)]):-
        M>1,jack_va_en_carromato(A,W,B).

camino(A,B,M,LI,CA,VIS,[etapa(W,no,no)|T]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),
        camino(W,B,M2,LI,CA,[W|VIS],T).
camino(A,B,M,LI,CA,VIS,[etapa(W,yes,no)|T]):-
        M>0,jack_pasa_por_callejon(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),LI_N is (LI-1),
        camino(W,B,M2,LI_N,CA,[W|VIS],T).
camino(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)|T]):-
        M>1,jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,M2 is (M-2),CA_N is (CA-1),
        camino(W2,B,M2,LI,CA_N,[[W,W2]|VIS],T).

jack_camina(A,B):-
        conectados(A,B),
        \+poli(_,A,B).
jack_pasa_por_callejon(A,B):-
        callejon(A,B).
jack_pasa_por_callejon(A,B):-
        callejon(B,A).
jack_va_en_carromato(A,B,C):-
        conectados(A,B),\+poli(_,A,B),
        conectados(B,C),\+poli(_,B,C).

conectados(A,B):-
        conexion(A,B).
conectados(A,B):-
        conexion(B,A).

procesa_etapa([]):-
        retract(jack_libre(yes)),
        assertz(jack_libre(no)),
        write("...... no me puedo moveeeer, me habeis pilladoooooooo"),nl.
procesa_etapa([etapa(D,no,no)|_]):-
        movimientos_que_quedan(M),
        posicion_jack(P),assertz(jack_ha_estado(P)),assertz(jack_ha_estado(D)),
        retract(posicion_jack(_)),assertz(posicion_jack(D)),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        write("...... ya me he movido.... bwahahahaha"),nl.
procesa_etapa([etapa(D,yes,no)|_]):-
        movimientos_que_quedan(M),
        linternas_que_quedan(LI),
        posicion_jack(P),assertz(jack_ha_estado(P)),assertz(jack_ha_estado(D)),
        retract(posicion_jack(_)),assertz(posicion_jack(D)),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        retract(linternas_que_quedan(_)),LI_N is LI-1,assertz(movimientos_que_quedan(LI_N)),
        write("...... ya me he movido, por un callejon!!!.... bwahahahaha.."),nl.
procesa_etapa([etapa(I,no,yes),etapa(D,no,yes)|_]):-
        movimientos_que_quedan(M),
        carromatos_que_quedan(CA),
        posicion_jack(P),assertz(jack_ha_estado(P)),assertz(jack_ha_estado(I)),assertz(jack_ha_estado(D)),
        retract(posicion_jack(_)),assertz(posicion_jack(D)),
        retract(movimientos_que_quedan(_)),M_N is M-2,assertz(movimientos_que_quedan(M_N)),
        retract(carromatos_que_quedan(_)),CA_N is CA-1,assertz(movimientos_que_quedan(CA_N)),
        write("...... ya me he movido, usando un carromato!!!.... bwahahahaha.."),nl.

/* Configuracion del juego */

/* prostituas  por noche */
cuantas_pes(1,5).
cuantas_pes(2,4).
cuantas_pes(3,3).
cuantas_pes(4,2).

/* posiciones de salida de las pes */
salida_pe(1,1).
salida_pe(2,1).
salida_pe(3,1).
salida_pe(4,1).
salida_pe(5,1).
salida_pe(6,1).
salida_pe(7,1).

/* descripcion del etapa */
conexion(1,2).
conexion(2,3).
conexion(3,4).
conexion(4,5).
poli(rojo,2,4).
callejon(2,4).
callejon(3,4).

linternas_por_noche(1,3).
linternas_por_noche(2,3).
linternas_por_noche(3,3).
linternas_por_noche(4,3).

carromatos_por_noche(1,2).
carromatos_por_noche(2,2).
carromatos_por_noche(3,2).
carromatos_por_noche(4,2).
