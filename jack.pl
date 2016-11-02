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
        abolish(posicion_jack,1),abolish(jack_ha_estado,1),assertz(posicion_jack(0)),assertz(jack_ha_estado(0)),abolish(rastro,1),assertz(rastro(0)),
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
        camino(P,G,M,LI,CA,[],CAM),!,
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa,
        polis_pueden_jugar.
/* si no hay un camino , intentar moverse a alguna de las posiciones
 * adyacentes y esperar a que los policias se muevan y despejen el paso */
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        write("mmhh.... esta chungo...!!!"),nl,
        posicion_jack(P),
        movimientos_que_quedan(M),
        M>0,
        linternas_que_quedan(LI),
        carromatos_que_quedan(CA),
        findall(C,(cx(P,C);cx(C,P)),LC),
        siguiente_cx(LC,CX,LC2),
        LC is LC2,
        camino(P,CX,1,LI,CA,[],CAM),!,
        procesa_etapa(CAM),
        avisa_si_en_guarida,
        avisa_si_jack_escapa,
        polis_pueden_jugar.
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
        jack_en_guarida,noche(N),N=4,write("me escapeeeeeeeeee he ganado!!!!!!, bwaahhahahahaaha"),nl,end.
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
        M>0,LI>0,jack_pasa_por_cj(A,B).
camino(A,B,M,_,CA,_,[etapa(W,no,yes),etapa(B,no,yes)]):-
        M>1,CA>0,jack_va_en_carromato(A,W,B).

/* los carromatos se pillan si jack esta rodeado (primero intentara ir a un sitio donde no haya polis, sino pues a uno donde lo haya) */
camino(A,B,M,LI,CA,VIS,[etapa(W,no,yes),etapa(W2,no,yes)]):-
        M>1,CA>0,cerca_polis(A),jack_va_en_carromato(A,W,W2),\+member(B,VIS),\+member(W,VIS),\+member(W2,VIS),\+ W=B,\+ W2=B,\+W2=A,M2 is (M-2),CA_N is (CA-1),
        append([A,W],VIS,VIS_N),camino(W2,B,M2,LI,CA_N,VIS_N,_).

camino(A,B,M,LI,CA,VIS,[etapa(W,no,no)]):-
        M>0,jack_camina(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI,CA,VIS_N,_).
camino(A,B,M,LI,CA,VIS,[etapa(W,yes,no)]):-
        M>0,LI>0,jack_pasa_por_cj(A,W),\+member(B,VIS),\+member(W,VIS),\+ W=B,M2 is (M-1),LI_N is (LI-1),
        append([A],VIS,VIS_N),camino(W,B,M2,LI_N,CA,VIS_N,_).

/* debug :cuando camino_dbg cambie, replicar anadiendo el Tail*/

jack_camina(A,B):-
        conectados(A,B),
        \+poli_enmedio(A,B).
jack_pasa_por_cj(A,B):-
        hay_callejon(A,B),\+jack_camina(A,B),!.
jack_va_en_carromato(A,B,C):-
        conectados(A,B),\+poli_enmedio(A,B),
        conectados(B,C),\+poli_enmedio(B,C).

conectados(A,B):-
        (cx(A,B);cx(B,A)).
hay_callejon(A,B):-
        (cj(A,B);cj(B,A)).
poli_enmedio(A,B):-
        (poli_en(_,A,B);poli_en(_,B,A)).

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
        write("...... ya me he movido, por un callejon!!!.... bwahahahaha.."),nl,!.
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
         \+(poli_en(P,C,_);poli_en(P,_,C)),
        write(".... listooooo, ahi no puedes mirar! :D"),nl.
examina_pista(P,C) :-
        jack_ha_estado(C),
        write(".... vaaaale, si he estado ahhi!!!!!"),nl,
        assertz(rastro(C)),
        retract(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),!.
examina_pista(_,_) :- write(" .... mmmmmmmhhh  no!! :D"),nl,fail.

cerca_polis(P):-
        muy_cerca_polis(P),!.
cerca_polis(P):-
        (cx(A,P);cx(P,A)),
        muy_cerca_polis(A),!.

muy_cerca_polis(P):-
        poli_en(_,P,_);poli_en(_,_,P).

max_loc(M):-
        bagof(N,cx(_,N),Ns),
        max_list(Ns,M).

siguiente_cx([],_,_):-fail.
siguiente_cx([H|T],H,T).


polis_inicio:- abolish(poli_en,3),abolish(poli_ha_jugado,2),
               forall(poli(X),(assertz(poli_en(X,0,0)),assertz(poli_ha_jugado(X,no)))).

polis_pueden_jugar:-
               abolish(poli_ha_jugado,2),forall(poli(X),assertz(poli_ha_jugado(X,no))).

st:-
        noche(N),write("noche:"),write(N),nl,
        movimientos_que_quedan(M),write("movimientos:"),write(M),nl,
        linternas_que_quedan(L),write("linternas:"),write(L),nl,
        carromatos_que_quedan(C),write("carromatos:"),write(C),nl,
        foreach(
                (rastro(C)),
                (write("rastro:"),write(C),nl)),
        foreach(
                (poli(P),poli_en(P,C1,C2),poli_ha_jugado(P,YN)),
                (write("poli:"),write(P),write(",jugado:"),write(YN),write(",entre:"),write(C1),write(",y:"),write(C2),nl)).

/* Configuracion del juego */
poli(r).
poli(v).
poli(az).
poli(am).
poli(m).

/* posiciones de salida de las pes */
salida_pe(1,3).
salida_pe(2,27).
salida_pe(4,65).
salida_pe(6,21).

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

/* cxes */
cx(1,2).
cx(1,24).
cx(1,6).
cx(1,26).
cx(1,28).
cx(1,8).
cx(1,9).
cx(2,26).
cx(2,26).
cx(2,28).
cx(2,8).
cx(2,9).
cx(2,11).
cx(2,3).
cx(3,9).
cx(3,11).
cx(3,4).
cx(3,5).
cx(4,11).
cx(4,12).
cx(4,5).
cx(5,12).
cx(5,13).
cx(5,15).
cx(5,16).
cx(5,17).
cx(6,24).
cx(6,25).
cx(6, 7).
cx(6,44).
cx(6,26).
cx(7,24).
cx(7,25).
cx(7,26).
cx(7,44).
cx(8,26).
cx(8,28).
cx(8, 9).
cx(8,10).
cx(9,10).
cx(9,11).
cx(10,30).
cx(11,12).
cx(11,30).
cx(12,13).
cx(12,30).
cx(13,14).
cx(13,15).
cx(13,16).
cx(13,17).
cx(13,30).
cx(13,32).
cx(14,30).
cx(14,31).
cx(14,32).
cx(14,33).
cx(14,34).
cx(14,52).
cx(14,54).
cx(15,16).
cx(15,17).
cx(15,33).
cx(15,34).
cx(15,35).
cx(15,36).
cx(16,17).
cx(16,33).
cx(16,36).
cx(17,18).
cx(17,36).
cx(17,38).
cx(18,19).
cx(18,20).
cx(18,36).
cx(18,38).
cx(18,39).
cx(19,20).
cx(19,39).
cx(20,40).
cx(21,40).
cx(21,41).
cx(21,42).
cx(21,23).
cx(22,42).
cx(22,23).
cx(22,77).
cx(23,77).
cx(24,26).
cx(24,25).
cx(24,44).
cx(24,43).
cx(24,59).
cx(25,26).
cx(25,44).
cx(25,43).
cx(25,59).
cx(26,28).
cx(26,44).
cx(26,79).
cx(26,46).
cx(26,27).
cx(27,44).
cx(27,79).
cx(27,46).
cx(27,28).
cx(27,45).
cx(27,47).
cx(27,48).
cx(27,29).
cx(28,46).
cx(28,45).
cx(28,47).
cx(28,48).
cx(28,29).
cx(29,46).
cx(29,45).
cx(29,47).
cx(29,48).
cx(29,30).
cx(29,49).
cx(29,64).
cx(29,66).
cx(29,50).
cx(30,49).
cx(30,64).
cx(30,66).
cx(30,50).
cx(30,32).
cx(31,50).
cx(31,32).
cx(31,51).
cx(31,52).
cx(31,32).
cx(31,54).
cx(31,33).
cx(32,33).
cx(32,52).
cx(32,54).
cx(33,52).
cx(33,54).
cx(33,36).
cx(33,34).
cx(33,35).
cx(33,36).
cx(34,54).
cx(34,53).
cx(34,68).
cx(34,55).
cx(34,35).
cx(34,36).
cx(34,37).
cx(35,36).
cx(35,54).
cx(35,53).
cx(35,68).
cx(35,55).
cx(35,37).
cx(36,38).
cx(37,54).
cx(37,53).
cx(37,68).
cx(37,55).
cx(37,38).
cx(37,39).
cx(38,39).
cx(39,56).
cx(40,41).
cx(40,57).
cx(40,73).
cx(40,58).
cx(40,42).
cx(40,41).
cx(41,57).
cx(41,73).
cx(41,58).
cx(41,42).
cx(42,57).
cx(42,73).
cx(42,58).
cx(43,44).
cx(43,59).
cx(44,59).
cx(44,46).
cx(44,79).
cx(45,48).
cx(45,47).
cx(45,61).
cx(46,79).
cx(46,47).
cx(46,48).
cx(47,61).
cx(47,48).
cx(48,49).
cx(48,64).
cx(48,63).
cx(48,62).
cx(49,62).
cx(49,63).
cx(49,64).
cx(49,50).
cx(49,66).
cx(50,64).
cx(50,66).
cx(50,52).
cx(50,51).
cx(51,64).
cx(51,66).
cx(55,65).
cx(51,84).
cx(51,67).
cx(51,52).
cx(52,67).
cx(52,53).
cx(52,54).
cx(53,67).
cx(53,54).
cx(53,68).
cx(53,55).
cx(54,68).
cx(54,55).
cx(55,68).
cx(55,86).
cx(55,56).
cx(56,68).
cx(56,86).
cx(56,57).
cx(57,73).
cx(57,58).
cx(57,76).
cx(57,75).
cx(57,74).
cx(58,73).
cx(58,76).
cx(58,75).
cx(58,74).
cx(59,60).
cx(59,95).
cx(59,96).
cx(59,78).
cx(60,95).
cx(60,96).
cx(60,78).
cx(60,79).


/* callejones */
cj(1,7).
cj(1,26).
cj(2,9).
cj(3,4).
cj(3,11).
cj(4,11).
cj(4,5).
cj(4,12).
cj(5,12).
cj(6,24).
cj(6,7).
cj(7,26).
cj(8,9).
cj(8,28).
cj(8,29).
cj(8,30).
cj(8,10).
cj(9,10).
cj(9,11).
cj(10,11).
cj(10,28).
cj(10,29).
cj(10,30).
cj(12,30).
cj(12,13).
cj(13,30).
cj(13,14).
cj(13,33).
cj(13,15).
cj(14,33).
cj(14,15).
cj(14,32).
cj(15,16).
cj(15,33).
cj(16,17).
cj(16,36).
cj(17,36).
cj(18,38).
cj(18,39).
cj(18,19).
cj(19,39).
cj(19,56).
cj(19,20).
cj(19,40).
cj(19,57).
cj(20,39).
cj(20,56).
cj(20,57).
cj(20,40).
cj(21,42).
cj(21,22).
cj(21,23).
cj(22,42).
cj(23,42).
cj(24,25).
cj(25,44).
cj(26,44).
cj(27,28).
cj(27,46).
cj(28,30).
cj(28,29).
cj(29,30).
cj(29,48).
cj(29,49).
cj(30,31).
cj(30,32).
cj(30,50).
cj(30,50).
cj(31,32).
cj(31,50).
cj(31,52).
cj(32,50).
cj(33,34).
cj(33,54).
cj(34,35).
cj(34,54).
cj(34,35).
cj(35,36).
cj(35,37).
cj(35,38).
cj(36,37).
cj(36,38).
cj(38,39).
cj(39,55).
cj(39,56).
cj(39,57).
cj(40,56).
cj(40,57).
cj(40,41).
