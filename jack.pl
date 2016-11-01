/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        banner,
        write("Sombras sobre Londres... v.1.1, (C) Jorge de Antonio,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        max_loc(M),
        repeat,
        random(1,M,G),
        puede_ser_guarida(G),
        abolish(guarida,1),assertz(guarida(G)),
        file_id(ID),
        write(ID,"guarida:"),write(ID,G),nl(ID),
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl,!.

otra_noche_mas :-
        jack_libre(yes),
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
        jack_en(CR),!.

mueve_jack :-
        jack_libre(yes),
        jack_en_guarida,
        write("Nooooooor que ya estoy en mi guarida!!! bwaahhahahahaaha"),nl,!.
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        movimientos_que_quedan(0),
        write("He agotado mi limite de movimientos me habeis pilladooooooooo!!!"),nl,end,
        retract(jack_libre(yes)),assertz(jack_libre(no)),!.
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        posicion_jack(P),
        guarida(G),
        movimientos_que_quedan(M),
        M>0,
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        camino(P,G,M,LI,CA,[],CAM),
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa,
        polis_pueden_jugar,!.
/* si no hay un camino , intentar moverse a alguna de las posiciones
 * adyacentes y esperar a que los policias se muevan y despejen el paso */
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        write("mmhh.... esta chungo...!!!"),nl,
        posicion_jack(P),
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        findall(C,(cnx(P,C);cnx(C,P)),LC),
        siguiente_cnx(LC,CX,LC2),
        LC is LC2,
        camino(P,CX,1,LI,CA,[],CAM),
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa,!.
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        write("No me puedo moveeeer, me habeis pilladoooooooooo!!!"),nl,end,
        retract(jack_libre(yes)),assertz(jack_libre(no)),!.


/* pon poli */
pp:-
        jack_libre(yes),
        write("poli:"),read(P),
        poli(P),
        retract(poli_en(P,_,_)),
        repeat,
        lee_poli_en(P).

arresto:-
        write("poli:"),read(P),
        poli(P),
        arresto(P).
arresto(P):-
        jack_libre(yes),
        poli_ha_jugado(P,yes),
        write("... listoooo que ese poli ya ha jugadoooo......"),nl,!.
arresto(P):-
        jack_libre(yes),
        poli_ha_jugado(P,no),
        write("donde:"),read(A),
        arresto(P,A).
arresto(_,A):-
        posicion_jack(C),
        A = C,
        write("... aaarggghhhh me habeis pilladooooooooooooooooo......"),nl,end,
        retract(jack_libre(yes)),
        assertz(jack_libre(no)),!.
arresto(P,_):- 
        write(" .... mmmmmmmhhh  no!! :D"),nl,
        retract(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),!.

pista:-
        write("poli:"),read(P),
        pista(P).

pista(P):-
        jack_libre(yes),
        poli_ha_jugado(P,yes),
        write("... listoooo que ese poli ya ha jugadoooo......"),nl.
pista(P):-
        jack_libre(yes),
        poli_ha_jugado(P,no),
        repeat,
        write("donde:"),read(C),
        examina_pista(P,C).

/* Funciones auxiliares .*/
init :-
        abolish(guarida,1),
        abolish(crime_scene,1),assertz(crime_scene(0)),
        abolish(jack_libre,1),assertz(jack_libre(yes)),
        abolish(posicion_jack,1),
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
        jack_en_guarida,noche(N),N=4,write("me escapeeeeeeeeee he ganado!!!!!!, bwaahhahahahaaha"),nl,end,
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
end:-
        write("       EEEEEEEEE    N       N   DDD                                     "),nl,
        write("       EEE          N N     N   DD D                                    "),nl,
        write("       EEE          N  N    N   DD  D                                   "),nl,
        write("       EEEEE        N   N   N   DD   D                                  "),nl,
        write("       EEE          N    N  N   DD  D                                   "),nl,
        write("       EEE          N     N N   DD D                                    "),nl,
        write("       EEEEEEEEE    N      N    DDD                                     "),nl,
        write("                                                                        "),nl.
 
/* movimiento */

camino(A,B,M,_,_,_,[etapa(B,no,no)]):-
        M>0,jack_camina(A,B).
camino(A,B,M,LI,_,_,[etapa(B,yes,no)]):-
        M>0,LI>0,jack_pasa_por_cllj(A,B).
camino(A,B,M,_,CA,_,[etapa(W,no,yes),etapa(B,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,B).

camino(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,\+muy_cerca_polis(W),\+cerca_polis(W2),M2 is (M-2),CA_N is (CA-1),
        append([A,W],VIS,VIS_N),camino(W2,B,M2,LI,CA_N,VIS_N,_).
camino(A,B,M,LI,CA,VIS,[etapa(W,no,no)]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI,CA,VIS_N,_).
camino(A,B,M,LI,CA,VIS,[etapa(W,yes,no)]):-
        M>0,LI>0,jack_pasa_por_cllj(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),LI_N is (LI-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI_N,CA,VIS_N,_).

/* debug :cuando camino cambie, replicar anadiendo el Tail*/
camino_dbg(A,B,M,_,_,_,[etapa(B,no,no)]):-
        M>0,jack_camina(A,B).
camino_dbg(A,B,M,LI,_,_,[etapa(B,yes,no)]):-
        M>0,LI>0,jack_pasa_por_cllj(A,B).
camino_dbg(A,B,M,_,CA,_,[etapa(W,no,yes),etapa(B,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,B).

camino_dbg(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)|T]):-
        M>1,CA>0,jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,\+muy_cerca_polis(W),\+cerca_polis(W2),M2 is (M-2),CA_N is (CA-1),
        append([A,W],VIS,VIS_N),camino_dbg(W2,B,M2,LI,CA_N,VIS_N,T).
camino_dbg(A,B,M,LI,CA,VIS,[etapa(W,no,no)|T]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),
        append([A],VIS,VIS_N),camino_dbg(W,B,M2,LI,CA,VIS_N,T).
camino_dbg(A,B,M,LI,CA,VIS,[etapa(W,yes,no)|T]):-
        M>0,LI>0,jack_pasa_por_cllj(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,\+muy_cerca_polis(W),M2 is (M-1),LI_N is (LI-1),
        append([A],VIS,VIS_N),camino_dbg(W,B,M2,LI_N,CA,VIS_N,T).


jack_camina(A,B):-
        conectados(A,B),
        \+poli_en(_,A,B).
jack_pasa_por_cllj(A,B):-
        cllj(A,B),\+jack_camina(A,B).
jack_pasa_por_cllj(A,B):-
        cllj(B,A),\+jack_camina(B,A).
jack_va_en_carromato(A,B,C):-
        conectados(A,B),\+poli_en(_,A,B),
        conectados(B,C),\+poli_en(_,B,C).

conectados(A,B):-
        cnx(A,B).
conectados(A,B):-
        cnx(B,A).

procesa_etapa([etapa(D,no,no)|_]):-
        movimientos_que_quedan(M),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        jack_en(D),
        write("...... ya me he movido.... bwahahahaha"),nl,!.
procesa_etapa([etapa(D,yes,no)|_]):-
        movimientos_que_quedan(M),
        linternas_que_quedan(LI),
        retract(movimientos_que_quedan(_)),M_N is M-1,assertz(movimientos_que_quedan(M_N)),
        retract(linternas_que_quedan(_)),LI_N is LI-1,assertz(linternas_que_quedan(LI_N)),
        jack_en(D),
        write("...... ya me he movido, por un cllj!!!.... bwahahahaha.."),nl,!.
procesa_etapa([etapa(I,no,yes),etapa(D,no,yes)|_]):-
        movimientos_que_quedan(M),
        carromatos_que_quedan(CA),
        retract(movimientos_que_quedan(_)),M_N is M-2,assertz(movimientos_que_quedan(M_N)),
        retract(carromatos_que_quedan(_)),CA_N is CA-1,assertz(carromatos_que_quedan(CA_N)),
        jack_en(I),
        jack_en(D),
        write("...... ya me he movido, usando un carromato!!!.... bwahahahaha.."),nl,!.

jack_en(P):-
        assertz(jack_ha_estado(P)),
        retract(posicion_jack(_)),assertz(posicion_jack(P)),
        file_id(ID),
        movimientos_que_quedan(M),linternas_que_quedan(L),carromatos_que_quedan(C),
        write(ID,"paso por:"),write(ID,P),write(ID," mov:"),write(ID,M),write(ID," lin:"),write(ID,L),write(ID," carr:"),write(ID,C),nl(ID),!.
        
lee_poli_en(P):-
        write("poli:"),write(P),nl,
        write("entre:"),
        read(C1),
        write("y:"),
        read(C2),
        pon_poli(P,C1,C2).

pon_poli(_,0,_):-!.
pon_poli(P,X,Y):-
        assertz(poli_en(P,X,Y)),fail.

examina_pista(P,0) :-
        retract(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),!.
examina_pista(P,C) :-
        jack_ha_estado(C),
        write(".... vaaaale, si he estado ahhi!!!!!"),nl,
        retract(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),!.
examina_pista(_,_) :- write(" .... mmmmmmmhhh  no!! :D"),nl,fail.

cerca_polis(P):-
        (poli_en(_,P,A);poli_en(_,A,P)),
        muy_cerca_polis(A),!.
cerca_polis(P):-
        (poli_en(_,P,A);poli_en(_,A,P)),
        (cnx(A,C);cnx(C,A)),
        muy_cerca_polis(C),!.

muy_cerca_polis(P):-
        poli_en(_,P,_);poli_en(_,_,P).

max_loc(M):-
        bagof(N,cnx(_,N),Ns),
        max_list(Ns,M).

siguiente_cnx([],_,_):-fail.
siguiente_cnx([H|T],H,T).


polis_inicio:- abolish(poli_en,3),abolish(poli_ha_jugado,2),
               forall(poli(X),(assertz(poli_en(X,0,0)),assertz(poli_ha_jugado(X,no)))).

polis_pueden_jugar:-
               abolish(poli_ha_jugado,2),forall(poli(X),assertz(poli_ha_jugado(X,no))).

st:-
        foreach(
                (poli(P),poli_en(P,C1,C2),poli_ha_jugado(P,YN)),
                (write("poli:"),write(P),write(",jugado:"),write(YN),write(",entre:"),write(C1),write(",y:"),write(C2),nl)),
        movimientos_que_quedan(M),write("movimientos:"),write(M),nl,
        linternas_que_quedan(L),write("linternas:"),write(L),nl,
        carromatos_que_quedan(C),write("carromatos:"),write(C),nl.

/* Configuracion del juego */
poli(r).
poli(v).
poli(az).
poli(am).
poli(m).

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

linternas_por_noche(1,2).
linternas_por_noche(2,2).
linternas_por_noche(3,1).
linternas_por_noche(4,1).

carromatos_por_noche(1,3).
carromatos_por_noche(2,2).
carromatos_por_noche(3,2).
carromatos_por_noche(4,1).


/* tablero */

/* cnxes */
cnx(1,2).
cnx(1,24).
cnx(1,6).
cnx(1,26).
cnx(1,28).
cnx(1,8).
cnx(1,9).

cnx(2,26).
cnx(2,26).
cnx(2,28).
cnx(2,8).
cnx(2,9).
cnx(2,11).
cnx(2,3).

cnx(3,9).
cnx(3,11).
cnx(3,4).
cnx(3,5).

cnx(4,11).
cnx(4,12).
cnx(4,5).

cnx(5,12).
cnx(5,13).
cnx(5,15).
cnx(5,16).
cnx(5,17).

cnx(6,24).
cnx(6,25).
cnx(6, 7).
cnx(6,44).
cnx(6,26).

cnx(7,24).
cnx(7,25).
cnx(7,26).
cnx(7,44).

cnx(8,26).
cnx(8,28).
cnx(8, 9).
cnx(8,10).

cnx(9,10).
cnx(9,11).

cnx(10,30).

cnx(11,12).
cnx(11,30).

cnx(12,13).
cnx(12,30).

cnx(13,14).
cnx(13,15).
cnx(13,16).
cnx(13,17).
cnx(13,30).
cnx(13,32).

cnx(14,30).
cnx(14,31).
cnx(14,32).
cnx(14,33).
cnx(14,34).
cnx(14,52).
cnx(14,54).

cnx(15,16).
cnx(15,17).
cnx(15,33).
cnx(15,34).
cnx(15,35).
cnx(15,36).

cnx(16,17).
cnx(16,33).
cnx(16,36).

cnx(17,18).
cnx(17,36).
cnx(17,38).

cnx(18,19).
cnx(18,20).
cnx(18,36).
cnx(18,38).
cnx(18,39).

cnx(19,20).
cnx(19,39).
cnx(20,40).
cnx(21,40).
cnx(21,41).
cnx(21,42).
cnx(21,23).

cnx(22,42).
cnx(22,23).
cnx(22,77).

cnx(23,77).

cnx(24,26).
cnx(24,25).
cnx(24,44).
cnx(24,43).
cnx(24,59).

cnx(25,26).
cnx(25,44).
cnx(25,43).
cnx(25,59).

cnx(26,28).
cnx(26,44).
cnx(26,79).
cnx(26,46).
cnx(26,27).

cnx(27,44).
cnx(27,79).
cnx(27,46).
cnx(27,28).
cnx(27,45).
cnx(27,47).
cnx(27,48).
cnx(27,29).

cnx(28,46).
cnx(28,45).
cnx(28,47).
cnx(28,48).
cnx(28,29).

cnx(29,46).
cnx(29,45).
cnx(29,47).
cnx(29,48).
cnx(29,30).
cnx(29,49).
cnx(29,64).
cnx(29,66).
cnx(29,50).

cnx(30,49).
cnx(30,64).
cnx(30,66).
cnx(30,50).
cnx(30,32).

cnx(31,50).
cnx(31,32).
cnx(31,51).
cnx(31,52).
cnx(31,32).
cnx(31,54).
cnx(31,33).

cnx(32,33).
cnx(32,52).
cnx(32,54).

cnx(33,52).
cnx(33,54).
cnx(33,36).
cnx(33,34).
cnx(33,35).
cnx(33,36).

cnx(34,54).
cnx(34,53).
cnx(34,68).
cnx(34,55).
cnx(34,35).
cnx(34,36).
cnx(34,37).

cnx(35,36).
cnx(35,54).
cnx(35,53).
cnx(35,68).
cnx(35,55).
cnx(35,37).

cnx(36,38).

cnx(37,54).
cnx(37,53).
cnx(37,68).
cnx(37,55).
cnx(37,38).
cnx(37,39).

cnx(38,39).

cnx(39,56).

cnx(40,41).
cnx(40,57).
cnx(40,73).
cnx(40,58).
cnx(40,42).
cnx(40,41).

cnx(41,57).
cnx(41,73).
cnx(41,58).
cnx(41,42).

cnx(42,57).
cnx(42,73).
cnx(42,58).
cnx(43,44).
cnx(43,59).
cnx(44,59).
cnx(44,46).
cnx(44,79).
cnx(45,48).
cnx(45,47).
cnx(45,61).
cnx(46,79).
cnx(46,47).
cnx(46,48).
cnx(47,61).
cnx(47,48).
cnx(48,49).
cnx(48,64).
cnx(48,63).
cnx(48,62).
cnx(49,62).
cnx(49,63).
cnx(49,64).
cnx(49,50).
cnx(49,66).
cnx(50,64).
cnx(50,66).
cnx(50,52).
cnx(50,51).

/* callejones */
cllj(1,7).
cllj(1,26).
cllj(2,9).
cllj(3,4).
cllj(3,11).
cllj(4,11).
cllj(4,5).
cllj(4,12).
cllj(5,12).
cllj(6,24).
cllj(6,7).
cllj(7,26).
cllj(8,9).
cllj(8,28).
cllj(8,29).
cllj(8,30).
cllj(8,10).
cllj(9,10).
cllj(9,11).
cllj(10,11).
cllj(10,28).
cllj(10,29).
cllj(10,30).
cllj(12,30).
cllj(12,13).
cllj(13,30).
cllj(13,14).
cllj(13,33).
cllj(13,15).
cllj(14,33).
cllj(14,15).
cllj(14,32).
cllj(15,16).
cllj(15,33).
cllj(16,17).
cllj(16,36).
cllj(17,36).
cllj(18,38).
cllj(18,39).
cllj(18,19).
cllj(19,39).
cllj(19,56).
cllj(19,20).
cllj(19,40).
cllj(19,57).
cllj(20,39).
cllj(20,56).
cllj(20,57).
cllj(20,40).
cllj(21,42).
cllj(21,22).
cllj(21,23).
cllj(22,42).
cllj(23,42).
cllj(24,25).
cllj(25,44).
cllj(26,44).
cllj(27,28).
cllj(27,46).
cllj(28,30).
cllj(28,29).
cllj(29,30).
cllj(29,48).
cllj(29,49).
cllj(30,31).
cllj(30,32).
cllj(30,50).
