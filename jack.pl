/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        banner,
        write("Sombras sobre Londres... v.0.1, (C) Jorge de Antonio,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        repeat,
        random(1,200,G),
        puede_ser_guarida(G),
        abolish(guarida,1),assertz(guarida(G)),
        file_id(ID),
        write(ID,"guarida:"),write(ID,G),nl(ID),
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl.

otra_noche_mas :-
        jack_libre(yes),
        queda_por_matar(yes),
        noche(N),retract(noche(_)),(N2 is N+1),assertz(noche(N2)),
        abolish(pe,2),assert(pe(1,0)),assert(pe(2,0)),assert(pe(3,0)),assert(pe(4,0)),assert(pe(5,0)),assert(pe(6,0)),assert(pe(7,0)),
        abolish(linternas_que_quedan,1),linternas_por_noche(N2,LI),assertz(linternas_que_quedan(LI)),
        abolish(carromatos_que_quedan,1),carromatos_por_noche(N2,CARR),assertz(carromatos_que_quedan(CARR)),
        abolish(movimientos_que_quedan,1),assertz(movimientos_que_quedan(15)),
        abolish(posicion_jack,1),abolish(jack_ha_estado,1),assertz(posicion_jack(0)),assertz(jack_ha_estado(0)),
        write(" .... comienza noche "),write(N2),write(",bwahahahaha...."),nl.

mata_una :-
        jack_mata_una(C),
        write(" .... Jack mata en.. "),write(C),write(", ... bwahahahaha"),nl,
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        jack_en(C).

mata_dos :-
        repeat,
        jack_mata_una(C),
        jack_mata_una(CC),
        \+C=CC,
        write(" .... Jack mata en.. "),write(C),write(", y "),write(CC),write(", ... bwahahahaha"),nl,
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        elige_donde_jack(C,CC,CR),
        jack_en(CR).

mueve_jack :-
        jack_libre(yes),
        queda_por_matar(yes),
        jack_en_guarida,
        write("Nooooooor que ya estoy en mi guarida!!! bwaahhahahahaaha"),nl.
mueve_jack :-
        jack_libre(yes),
        queda_por_matar(yes),
        \+ jack_en_guarida,
        movimientos_que_quedan(0),
        write("He agotado mi limite de movimientos me habeis pilladooooooooo!!!"),nl,
        retract(jack_libre(yes)),assertz(jack_libre(no)).
mueve_jack :-
        jack_libre(yes),
        queda_por_matar(yes),
        \+ jack_en_guarida,
        posicion_jack(P),
        guarida(G),
        movimientos_que_quedan(M),
        M>0,
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        camino(P,G,M,LI,CA,[],CAM),!,
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa.

donde_poli(P,A,B) :-
        jack_libre(yes),
        queda_por_matar(yes),
        retract(poli(P,_,_)),assertz(poli(P,A,B)).

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
        write(".... vaaaale, si he estado ahhi!!!!!"),nl.
pista(_) :- write(" .... mmmmmmmhhh  no!! :D"),nl.


/* Funciones auxiliares .*/
init :-
        abolish(guarida,1),
        abolish(crime_scene,1),assertz(crime_scene(0)),
        abolish(jack_libre,1),assertz(jack_libre(yes)),
        abolish(posicion_jack,1),
        abolish(queda_por_matar,1),assertz(queda_por_matar(yes)),
        abolish(noche,1),
        abolish(jack_ha_estado,1),
        open('jack.txt',write,ID,[type(text),buffer(false)]),abolish(file_id,1),assertz(file_id(ID)),write(ID,"jack!"),nl(ID),
        assertz(noche(0)).

puede_ser_guarida(G):-
        \+salida_pe(_,G).

jack_mata_una(C) :-
        repeat,
        random(1,8,P),
        salida_pe(P,C),
        \+crime_scene(C),
        file_id(ID),write(ID,"mato en:"),write(ID,C),nl(ID).

elige_donde_jack(C,CC,RC) :-
        random(0,1,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

jack_en_guarida :- posicion_jack(C),guarida(C).

avisa_si_en_guarida :-
        jack_en_guarida,write("Llegue a mi guarida, bwaahhahahahaaha"),nl.
avisa_si_en_guarida.
avisa_si_jack_escapa :-
        jack_en_guarida,noche(N),N=4,write("me escapeeeeeeeeee he ganado!!!!!!, bwaahhahahahaaha"),nl.
avisa_si_jack_escapa.


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

camino(A,B,M,_,_,_,[etapa(B,no,no)]):-
        M>0,jack_camina(A,B).
camino(A,B,M,LI,_,_,[etapa(B,yes,no)]):-
        M>0,LI>0,jack_pasa_por_callejon(A,B).
camino(A,B,M,_,CA,_,[etapa(W,no,yes),etapa(B,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,B).

camino(A,B,M,LI,CA,VIS,[etapa(W,no,no)]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),
        camino(W,B,M2,LI,CA,[[A,W]|VIS],_).
camino(A,B,M,LI,CA,VIS,[etapa(W,yes,no)]):-
        M>0,LI>0,jack_pasa_por_callejon(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),LI_N is (LI-1),
        camino(W,B,M2,LI_N,CA,[[A,W]|VIS],_).
camino(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,M2 is (M-2),CA_N is (CA-1),
        camino(W2,B,M2,LI,CA_N,[[A,W,W2]|VIS],_).

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
        jack_en(D),
        movimientos_que_quedan(M),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        write("...... ya me he movido.... bwahahahaha"),nl.
procesa_etapa([etapa(D,yes,no)|_]):-
        jack_en(D),
        movimientos_que_quedan(M),
        linternas_que_quedan(LI),
        retract(posicion_jack(_)),assertz(posicion_jack(D)),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        retract(linternas_que_quedan(_)),LI_N is LI-1,assertz(movimientos_que_quedan(LI_N)),
        write("...... ya me he movido, por un callejon!!!.... bwahahahaha.."),nl.
procesa_etapa([etapa(I,no,yes),etapa(D,no,yes)|_]):-
        jack_en(I),
        jack_en(D),
        movimientos_que_quedan(M),
        carromatos_que_quedan(CA),
        retract(movimientos_que_quedan(_)),M_N is M-2,assertz(movimientos_que_quedan(M_N)),
        retract(carromatos_que_quedan(_)),CA_N is CA-1,assertz(movimientos_que_quedan(CA_N)),
        write("...... ya me he movido, usando un carromato!!!.... bwahahahaha.."),nl.

jack_en(P):-
        assertz(jack_ha_estado(P)),
        retract(posicion_jack(_)),assertz(posicion_jack(P)),
        file_id(ID),
        write(ID,"paso por:"),write(ID,P),nl(ID).
        
/* Configuracion del juego */

/* prostituas  por noche */
cuantas_pes(1,5).
cuantas_pes(2,4).
cuantas_pes(3,3).
cuantas_pes(4,2).

/* posiciones de salida de las pes */
salida_pe(1,3).
salida_pe(2,27).
salida_pe(3,149).
salida_pe(4,65).
salida_pe(5,84).
salida_pe(6,21).
salida_pe(7,158).
salida_pe(8,147).

/* descripcion del etapa */
poli(rojo,0,0).
poli(verde,0,0).
poli(azul,0,0).
poli(amarillo,0,0).
poli(marron,0,0).

linternas_por_noche(1,3).
linternas_por_noche(2,3).
linternas_por_noche(3,3).
linternas_por_noche(4,3).

carromatos_por_noche(1,2).
carromatos_por_noche(2,2).
carromatos_por_noche(3,2).
carromatos_por_noche(4,2).


/* tablero */

/* conexiones */
conexion(1,2).
conexion(1,24).
conexion(1,6).
conexion(1,26).
conexion(1,28).
conexion(1,8).
conexion(1,9).

conexion(2,26).
conexion(2,26).
conexion(2,28).
conexion(2,8).
conexion(2,9).
conexion(2,11).
conexion(2,3).

conexion(3,9).
conexion(3,11).
conexion(3,4).
conexion(3,5).

conexion(4,11).
conexion(4,12).
conexion(4,5).

conexion(5,12).
conexion(5,13).
conexion(5,15).
conexion(5,16).
conexion(5,17).

conexion(6,24).
conexion(6,25).
conexion(6, 7).
conexion(6,44).
conexion(6,26).

conexion(7,24).
conexion(7,25).
conexion(7,26).
conexion(7,44).

conexion(8,26).
conexion(8,28).
conexion(8, 9).
conexion(8,10).

conexion(9,10).
conexion(9,11).

conexion(10,30).

conexion(11,12).
conexion(11,30).

conexion(12,13).
conexion(12,30).

conexion(13,14).
conexion(13,15).
conexion(13,16).
conexion(13,17).
conexion(13,30).
conexion(13,32).

conexion(14,30).
conexion(14,31).
conexion(14,32).
conexion(14,33).
conexion(14,34).
conexion(14,52).
conexion(14,54).

conexion(15,16).
conexion(15,17).
conexion(15,33).
conexion(15,34).
conexion(15,35).
conexion(15,36).

conexion(16,17).
conexion(16,33).
conexion(16,36).

conexion(17,18).
conexion(17,36).
conexion(17,38).

conexion(18,19).
conexion(18,20).
conexion(18,36).
conexion(18,38).
conexion(18,39).

conexion(19,20).
conexion(19,39).

/* callejones */
callejon(1,26).
callejon(3,11).
