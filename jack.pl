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
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl,!.

otra_noche_mas :-
        jack_libre(yes),
        queda_por_matar(yes),
        noche(N),retract(noche(_)),(N2 is N+1),assertz(noche(N2)),
        abolish(pe,2),assert(pe(1,0)),assert(pe(2,0)),assert(pe(3,0)),assert(pe(4,0)),assert(pe(5,0)),assert(pe(6,0)),assert(pe(7,0)),
        abolish(linternas_que_quedan,1),linternas_por_noche(N2,LI),assertz(linternas_que_quedan(LI)),
        abolish(carromatos_que_quedan,1),carromatos_por_noche(N2,CARR),assertz(carromatos_que_quedan(CARR)),
        abolish(movimientos_que_quedan,1),assertz(movimientos_que_quedan(15)),
        abolish(posicion_jack,1),abolish(jack_ha_estado,1),assertz(posicion_jack(0)),assertz(jack_ha_estado(0)),
        write(" .... comienza noche "),write(N2),write(",bwahahahaha...."),nl,
        file_id(ID),write(ID,"noche:"),write(ID,N2),nl(ID).

mata_una :-
        jack_mata_una(C),
        write(" .... Jack mata en.. "),write(C),write(", ... bwahahahaha"),nl,
        write("Polis, dejad de comer donuts (y mear) y moveos!!!"),nl,
        jack_en(C),!.

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
/* si no hay un camino , intentar moverse a alguna de las posiciones
 * adyacentes y esperar a que los policias se muevan y despejen el paso */
mueve_jack :-
        jack_libre(yes),
        queda_por_matar(yes),
        \+ jack_en_guarida,
        posicion_jack(P),
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        repeat,
        conexion(P,X),
        camino(P,X,1,LI,CA,[],CAM),!,
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa.
mueve_jack :-
        jack_libre(yes),
        queda_por_matar(yes),
        \+ jack_en_guarida,
        write("No me puedo moveeeer, me habeis pilladoooooooooo!!!"),nl,
        retract(jack_libre(yes)),assertz(jack_libre(no)).


pon_poli:-
        jack_libre(yes),
        queda_por_matar(yes),
        repeat,
        write("poli:"),read(P),
        retract(poli(P,_,_)),
        lee_poli(P).

arresto(C):-
        jack_libre(yes),
        queda_por_matar(yes),
        posicion_jack(C),
        write("... aaarggghhhh me habeis pilladooooooooooooooooo......"),
        retract(jack_libre(yes)),
        assertz(jack_libre(no)).

pista:-
        jack_libre(yes),
        queda_por_matar(yes),
        repeat,
        write("donde:"),read(C),
        examina_pista(C),
        fail.

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
        polis_inicio,
        assertz(noche(0)).

puede_ser_guarida(G):-
        \+salida_pe(_,G).

jack_mata_una(C) :-
        repeat,
        random(1,8,P),
        salida_pe(P,C),
        \+crime_scene(C),
        \+cerca_polis(C),
        file_id(ID),write(ID,"mato en:"),write(ID,C),nl(ID).

matanza(P) :-
        salida_pe(P,C),
        file_id(ID),write(ID,"mato por mandato en:"),write(ID,C),nl(ID).

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

camino(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,\+muy_cerca_polis(W),\+cerca_polis(W2),M2 is (M-2),CA_N is (CA-1),
        append([A,W],VIS,VIS_N),camino(W2,B,M2,LI,CA_N,VIS_N,_).
camino(A,B,M,LI,CA,VIS,[etapa(W,no,no)]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI,CA,VIS_N,_).
camino(A,B,M,LI,CA,VIS,[etapa(W,yes,no)]):-
        M>0,LI>0,jack_pasa_por_callejon(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),LI_N is (LI-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI_N,CA,VIS_N,_).

jack_camina(A,B):-
        conectados(A,B),
        \+poli(_,A,B).
jack_pasa_por_callejon(A,B):-
        callejon(A,B),\+jack_camina(A,B).
jack_pasa_por_callejon(A,B):-
        callejon(B,A),\+jack_camina(B,A).
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
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        jack_en(D),
        write("...... ya me he movido.... bwahahahaha"),nl.
procesa_etapa([etapa(D,yes,no)|_]):-
        movimientos_que_quedan(M),
        linternas_que_quedan(LI),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        retract(linternas_que_quedan(_)),LI_N is LI-1,assertz(movimientos_que_quedan(LI_N)),
        jack_en(D),
        write("...... ya me he movido, por un callejon!!!.... bwahahahaha.."),nl.
procesa_etapa([etapa(I,no,yes),etapa(D,no,yes)|_]):-
        movimientos_que_quedan(M),
        carromatos_que_quedan(CA),
        retract(movimientos_que_quedan(_)),M_N is M-2,assertz(movimientos_que_quedan(M_N)),
        retract(carromatos_que_quedan(_)),CA_N is CA-1,assertz(movimientos_que_quedan(CA_N)),
        jack_en(I),
        jack_en(D),
        write("...... ya me he movido, usando un carromato!!!.... bwahahahaha.."),nl.

jack_en(P):-
        assertz(jack_ha_estado(P)),
        retract(posicion_jack(_)),assertz(posicion_jack(P)),
        file_id(ID),
        movimientos_que_quedan(M),linternas_que_quedan(L),carromatos_que_quedan(C),
        write(ID,"paso por:"),write(ID,P),write(ID,"mov:"),write(ID,M),write(ID,"lin:"),write(ID,L),write(ID,"carr:"),write(ID,C),nl(ID).
        
lee_poli(P):-
        write("poli:"),write(P),nl,
        write("en:"),
        read(C1),
        write("y:"),
        read(C2),
        assertz(poli(P,C1,C2)),
        fail.

examina_pista(C) :-
        jack_ha_estado(C),
        write(".... vaaaale, si he estado ahhi!!!!!"),nl,!.
examina_pista(_) :- write(" .... mmmmmmmhhh  no!! :D"),nl.

cerca_polis(P):-
        (poli(_,P,A);poli(_,A,P)),
        muy_cerca_polis(A).
cerca_polis(P):-
        (poli(_,P,A);poli(_,A,P)),
        (conexion(A,C);conexion(C,A)),
        muy_cerca_polis(C).

muy_cerca_polis(P):-
        poli(_,P,_);poli(_,_,P).

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
polis_inicio:-
        assertz(poli(rojo,0,0)),
        assertz(poli(verde,0,0)),
        assertz(poli(azul,0,0)),
        assertz(poli(amarillo,0,0)),
        assertz(poli(marron,0,0)).

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
conexion(20,40).
conexion(21,40).
conexion(21,41).
conexion(21,42).
conexion(21,23).

conexion(22,42).
conexion(22,23).
conexion(22,77).

conexion(23,77).

conexion(24,26).
conexion(24,25).
conexion(24,44).
conexion(24,43).
conexion(24,59).

conexion(25,26).
conexion(25,44).
conexion(25,43).
conexion(25,59).

conexion(26,28).
conexion(26,44).
conexion(26,79).
conexion(26,46).
conexion(26,27).

conexion(27,44).
conexion(27,79).
conexion(27,46).
conexion(27,28).
conexion(27,45).
conexion(27,47).
conexion(27,48).
conexion(27,29).

conexion(28,46).
conexion(28,45).
conexion(28,47).
conexion(28,48).
conexion(28,29).

conexion(29,46).
conexion(29,45).
conexion(29,47).
conexion(29,48).
conexion(29,30).
conexion(29,49).
conexion(29,64).
conexion(29,66).
conexion(29,50).

conexion(30,49).
conexion(30,64).
conexion(30,66).
conexion(30,50).
conexion(30,32).

conexion(31,50).
conexion(31,32).
conexion(31,51).
conexion(31,52).
conexion(31,32).
conexion(31,54).
conexion(31,33).

conexion(32,33).
conexion(32,52).
conexion(32,54).

conexion(33,52).
conexion(33,54).
conexion(33,36).
conexion(33,34).
conexion(33,35).
conexion(33,36).

conexion(34,54).
conexion(34,53).
conexion(34,68).
conexion(34,55).
conexion(34,35).
conexion(34,36).
conexion(34,37).

conexion(35,36).
conexion(35,54).
conexion(35,53).
conexion(35,68).
conexion(35,55).
conexion(35,37).

conexion(36,38).

conexion(37,54).
conexion(37,53).
conexion(37,68).
conexion(37,55).
conexion(37,38).
conexion(37,39).

conexion(38,39).

conexion(39,56).

conexion(40,41).
conexion(40,57).
conexion(40,73).
conexion(40,58).
conexion(40,42).
conexion(40,41).

conexion(41,57).
conexion(41,73).
conexion(41,58).
conexion(41,42).

/* callejones */
callejon(1,7).
callejon(1,26).
callejon(2,9).
callejon(3,4).
callejon(3,11).
callejon(4,11).
callejon(4,5).
callejon(4,12).
callejon(5,12).
callejon(6,24).
callejon(6,7).
callejon(7,26).
callejon(8,9).
callejon(8,28).
callejon(8,29).
callejon(8,30).
callejon(8,10).
callejon(9,10).
callejon(9,11).
callejon(10,11).
callejon(10,28).
callejon(10,29).
callejon(10,30).
callejon(12,30).
callejon(12,13).
callejon(13,30).
callejon(13,14).
callejon(13,33).
callejon(13,15).
callejon(14,33).
callejon(14,15).
callejon(14,32).
callejon(15,16).
callejon(15,33).
callejon(16,17).
callejon(16,36).
callejon(17,36).
callejon(18,38).
callejon(18,39).
callejon(18,19).
callejon(19,39).
callejon(19,56).
callejon(19,20).
callejon(19,40).
callejon(19,57).
callejon(20,39).
callejon(20,56).
callejon(20,57).
callejon(20,40).
