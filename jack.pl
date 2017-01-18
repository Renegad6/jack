/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        banner,
        write("Sombras sobre Londres... v.3.0, (C) Jorge de Antonio, Fernando Noguera y JC,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        max_loc(M),
        repeat,
        random(1,M,G),
        puede_ser_guarida(G),
        assertz(guarida(G)),
        file_id(ID),
        write(ID,"guarida:"),write(ID,G),nl(ID),
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl,!.

otra_noche_mas :-
        jack_libre(yes),
        noche(N),retractall(noche(_)),(N2 is N+1),assertz(noche(N2)),
        file_id(ID),write(ID,"noche:"),write(ID,N2),nl(ID),
        abolish(pe,2),assert(pe(1,0)),assert(pe(2,0)),assert(pe(3,0)),assert(pe(4,0)),assert(pe(5,0)),assert(pe(6,0)),assert(pe(7,0)),
        abolish(linternas_que_quedan,1),linternas_por_noche(N2,LI),assertz(linternas_que_quedan(LI)),
        abolish(carromatos_que_quedan,1),carromatos_por_noche(N2,CARR),assertz(carromatos_que_quedan(CARR)),
        abolish(movimientos_que_quedan,1),assertz(movimientos_que_quedan(15)),
        abolish(posicion_jack,1),abolish(jack_ha_estado,1),assertz(posicion_jack(0)),assertz(jack_ha_estado(0)),abolish(rastro,1),assertz(rastro(0)),
        write(" .... comienza noche "),write(N2),write(",bwahahahaha...."),nl,
        mata(N2),!.

mata(N):-
        N\=3,mata_una.
mata(N):-
        N=3,mata_dos.
mata_una :-
        jack_mata_una(C),
        write(" .... Jack mata en.. "),write(C),write(", ... bwahahahaha"),nl,
        write("Polis, moveos!!!"),nl,
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
        retractall(jack_libre(yes)),assertz(jack_libre(no)),!.
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        posicion_jack(P),
        movimiento(P),
        avisa_si_en_guarida,
        avisa_si_jack_escapa,
        polis_pueden_jugar,!.
mueve_jack :-
        jack_libre(yes),
        \+ jack_en_guarida,
        write("No me puedo moveeeer, me habeis pilladoooooooooo!!!"),nl,end,
        retractall(jack_libre(yes)),assertz(jack_libre(no)),!.


/* pon poli */
pp:-
        jack_libre(yes),
        write("poli:"),read(P),
        poli(P),
        retractall(poli_en(P,_,_)),
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
        retractall(jack_libre(yes)),
        assertz(jack_libre(no)),!.
arresto(P,_):- 
        write(" .... mmmmmmmhhh  no!! :D"),nl,
        retractall(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),!.

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

reset(P):-
        retract(poli_ha_jugado(P,yes)),assertz(poli_ha_jugado(P,no)),!.
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
        assertz(noche(0)),
        carga_tablero,
        carga_callejones,
        elige_guarida.

puede_ser_guarida(G):-
        \+salida_pe(_,G).

jack_mata_una(H) :-
        findall(CC,
                (salida_pe(_,CC),\+crime_scene(CC),\+polis_cerca(CC),random(0,2,1)),L),
        \+length(L,0),
        L = [H|_],
        assertz(crime_scene(H)),
        file_id(ID),write(ID,"mato en:"),write(ID,H),nl(ID),!.
jack_mata_una(C) :-
        repeat,
        random(1,9,P),
        salida_pe(P,C),\+crime_scene(C),
        assertz(crime_scene(C)),
        file_id(ID),write(ID,"...mato en:"),write(ID,C),nl(ID),!.

matanza(P) :-
        salida_pe(P,C),
        file_id(ID),write(ID,"mato por mandato en:"),write(ID,C),nl(ID).

elige_donde_jack(C,CC,RC) :-
        random(0,2,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

jack_en_guarida :- posicion_jack(C),guarida(C).

avisa_si_en_guarida :-
        jack_en_guarida,write("Llegue a mi guarida, bwaahhahahahaaha"),nl,!.
avisa_si_en_guarida.
avisa_si_jack_escapa :-
        jack_en_guarida,noche(N),N=4,write("me escapeeeeeeeeee he ganado!!!!!!, bwaahhahahahaaha"),nl,end.
avisa_si_jack_escapa:-
        movimientos_que_quedan(M),M=0,\+jack_en_guarida,
        write("He agotado mi limite de movimientos me habeis pilladooooooooo!!!"),nl,end,
        retractall(jack_libre(yes)),assertz(jack_libre(no)),!.
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

/* 0. No me puedo mover porque ya llegue */
movimiento(A):-
        movimientos_que_quedan(M),
        M>0,
        guarida(G), 
        movimiento(A,G,M),!,
        writeln(".... ya me he movido!! bwahahahaha").

/* 1. Moverse rapido en carromato */
/* solo moverte lejos de los polis si quedan muchos movimientos, sino los polis
 * te puede ir alejando hasta que ya no puedas volver */
movimiento(A,G,M):-
        polis_al_lado(A),
        carromatos_que_quedan(C),
        C>0,
        M>9,
        M_N is M-2,
        findall(X-W-B,
                (jack_va_en_carromato(A,W,B),
                 encuentra_primero(M_N,A,B,G,X)),
                 L),
        minim(L,_-WW-BB),
        writeln(".... uso un carromato!! bwahahahaha"),
        retract(carromatos_que_quedan(C)),C_N is C-1,assertz(carromatos_que_quedan(C_N)),
        retract(movimientos_que_quedan(M)),assertz(movimientos_que_quedan(M_N)),
        jack_en(WW),
        jack_en(BB).

/* 2. Caminar acercandome a la guarida sin pasar al lado de polis */
movimiento(A,G,M):-
        M>9,
        M_N is M-1,
        findall(X-B-B,
                (jack_camina(A,B),
                 \+polis_al_lado(B),
                 encuentra_primero(M_N,A,B,G,X)),
                 L),
        minim(L,_-_-BB),
        retract(movimientos_que_quedan(_)),assertz(movimientos_que_quedan(M_N)),
        writeln(".... (2.1) in da hoood!! bwahahahaha"),
        jack_en(BB).

/* 3. Pasar por callejon sin pasar cerca de polis */
movimiento(A,G,M):-
        M>0,
        linternas_que_quedan(LI),
        LI>0,
        M_N is M-1,
        findall(X-B-B,
                (jack_pasa_por_cj(A,B),\+B=G,
                 \+polis_al_lado(B),
                 encuentra_primero(M_N,A,B,G,X)),
                 L),
        minim(L,_-_-BB),
        writeln(".... uso una linterna!! bwahahahaha"),
        retract(linternas_que_quedan(LI)),assertz(linternas_que_quedan(LI_N)),
        retract(movimientos_que_quedan(M)),assertz(movimientos_que_quedan(M_N)),
        jack_en(BB).

/* 4. Me muevo por un callejon aunque salga al lado de un poli */
movimiento(A,G,M):-
        M>0,
        linternas_que_quedan(LI),
        LI>0,
        M_N is M-1,
        findall(X-B-B,
                (jack_pasa_por_cj(A,B),\+B=G,
                 encuentra_primero(M_N,A,B,G,X)),
                 L),
        minim(L,_-_-BB),
        writeln(".... uso una linterna!! bwahahahaha"),
        retract(linternas_que_quedan(LI)),assertz(linternas_que_quedan(LI_N)),
        retract(movimientos_que_quedan(M)),assertz(movimientos_que_quedan(M_N)),
        jack_en(BB).

/* 5. Camino aunque pase al lado de un poli */
movimiento(A,G,M):-
        M>0,
        M_N is M-1,
        findall(X-B-B,
                (jack_camina(A,B),
                 encuentra_primero(M_N,A,B,G,X)),
                 L),
        minim(L,XX-_-BB),
        retract(movimientos_que_quedan(_)),assertz(movimientos_que_quedan(M_N)),
        write(".... (5.1) in da hoood:"),writeln(XX),
        jack_en(BB).

/* movimiento azar acercandome a la guarida */
movimiento(A,G,M):-
        M>0,
        M_N is M-1,
        findall(X-B-B,
                (jack_camina(A,B),
                 encuentra_primero(100,A,B,G,X)),
                 L),
        minim(L,XX-_-BB),
        retract(movimientos_que_quedan(_)),assertz(movimientos_que_quedan(M_N)),
        writeln(".... que chungo...."),
        jack_en(BB).

puedo_llegar(A,B,G,M):-
        noche(N),
        (N=4;M<10;polis_cerca(A)),
        B=G,!.
puedo_llegar(A,B,G,M):-
        \+B=G,
        camino(B,G,M),!.

camino(A,B,M):-
        M>0,distancia(A,B,D),D=<M,!.

jack_camina(A,B):-
        conectados(A,B),
        \+poli_enmedio(A,B).
jack_pasa_por_cj(A,B):-
        hay_callejon(A,B).
jack_va_en_carromato(A,B,C):-
        \+A=C,
        conectados(A,B),\+poli_enmedio(A,B),
        conectados(B,C),\+poli_enmedio(B,C),
        \+jack_camina(A,C).

conectados(A,B):-
        (cx(A,B);cx(B,A)).
hay_callejon(A,B):-
        (cj(A,B);cj(B,A)).
poli_enmedio(A,B):-
        (poli_en(_,A,B);poli_en(_,B,A)).

jack_en(P):-
        assertz(jack_ha_estado(P)),
        retractall(posicion_jack(_)),assertz(posicion_jack(P)),
        file_id(ID),
        movimientos_que_quedan(M),linternas_que_quedan(L),carromatos_que_quedan(C),
        write(ID,"paso por:"),write(ID,P),write(ID," mov:"),write(ID,M),write(ID," lin:"),write(ID,L),write(ID," carr:"),write(ID,C),nl(ID),!.
        
lee_poli_en(P):-
        write("entre:"),
        read(A),
        write("y:"),
        read(B),
        write("poli en:"),write(A),write(","),write(B),nl,assertz(poli_en(P,A,B)),
/* 1.determinar perimetro */
        foreach((conectados(X,A),conectados(X,B),\+poli_en(P,X,A)),
                (write("poli en:"),write(X),write(","),write(A),nl,assertz(poli_en(P,X,A)))),
        foreach((conectados(X,A),conectados(X,B),\+poli_en(P,X,B)),
                (write("poli en:"),write(X),write(","),write(B),nl,assertz(poli_en(P,X,B)))),
/* 2.todos con todos */
        foreach(poli_en(P,C1,_),
                foreach((poli_en(P,C2,_),\+C1=C2,\+poli_en(P,C1,C2)),
                        (write("poli en:"),write(C1),write(","),write(C2),nl,assertz(poli_en(P,C1,C2))))).

examina_pista(P,0):-
        retractall(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)).
examina_pista(P,C) :-
         \+(poli_en(P,C,_);poli_en(P,_,C)),
        write(".... listooooo, ahi no puedes mirar! :D"),nl,fail.
examina_pista(P,C) :-
        (poli_en(P,C,_);poli_en(P,_,C)),
        jack_ha_estado(C),
        write(".... vaaaale, si he estado ahhi!!!!!"),nl,
        retractall(poli_ha_jugado(P,no)),assertz(poli_ha_jugado(P,yes)),
        assertz(rastro(C)).
examina_pista(P,C) :- 
         (poli_en(P,C,_);poli_en(P,_,C)),!,
         \+jack_ha_estado(C),
         write(" .... mmmmmmmhhh  no!! :D"),nl,fail.

polis_cerca(P):-
        polis_al_lado(P),!.
polis_cerca(P):-
        (cx(A,P);cx(P,A)),
        polis_al_lado(A),!.

polis_al_lado(P):-
        poli_en(_,P,_);poli_en(_,_,P).

max_loc(M):-
        findall(N,cx(_,N),Ns),
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
                rastro(C),
                (write("rastro:"),writeln(C))),
        foreach(
                (poli(P),poli_en(P,C1,C2),poli_ha_jugado(P,YN)),
                (write("poli:"),write(P),write(",jugado:"),write(YN),write(",entre:"),write(C1),write(",y:"),write(C2),nl)),
        foreach(
                crime_scene(C),
                (write("crime_scene:"),writeln(C))).
minim([],_):-fail.
minim([A-B-C],A-B-C).
minim([A-B-C|T],A-B-C):-minim(T,D-_-_),(A=<D),!.
minim([A-_-_|T],D-E-F):-minim(T,D-E-F),(D=<A),!.

/* bucle de 1..num mov. hasta que encuentre el primero con el que puede llegar
 * a la guarida */
encuentra_primero(M,A,B,G,H):-
        M<5,
        numlist(1,M,NL),
        encuentra_primero_l(NL,A,B,G,H),!.
encuentra_primero(M,A,B,G,H):-
        M>=5,
        numlist(1,5,NL),
        encuentra_primero_l(NL,A,B,G,H),!.
encuentra_primero(M,A,B,G,H):-
        M>=5,
        encuentra_primero_l([M],A,B,G,H),!.
encuentra_primero_l([H|_],A,B,G,H):-
        puedo_llegar(A,B,G,H),!.
encuentra_primero_l([_|T],A,B,G,HH):-
        encuentra_primero_l(T,A,B,G,HH),!. 

vecindario(A,B):-
        puedo_llegar(A,A,B,5),
        writeln(".... in da hoood!! bwahahahaha"),!.

/* Configuracion del juego */
carga_tablero:-
    writeln('Cargando tablero...'),
    open('jack_cnx.txt',read,IDT,[type(text),buffer(false)]),
    repeat,             % try again forever
    read(IDT,X),
    add_cx(X),
    X == end_of_file,   % fail (backtrack) if not end of 
    !,
    close(IDT).
carga_distancias:-
    writeln('Cargando distancias...'),
    open('jack_rel.txt',read,IDT,[type(text),buffer(false)]),
    repeat,             % try again forever
    read(IDT,X),
    add_rel(X),
    X == end_of_file,   % fail (backtrack) if not end of 
    !,
    close(IDT)
carga_callejones:-
    writeln('Cargando callejones...'),
    open('jack_cj.txt',read,IDT,[type(text),buffer(false)]),
    repeat,             % try again forever
    read(IDT,X),
    add_cj(X),
    X == end_of_file,   % fail (backtrack) if not end of 
    !,
    close(IDT).

add_cx(X):-
        X==end_of_file.
add_cx(X):-
    \+ X==end_of_file,
    X = (A,B),
    assertz(cx(A,B)).

add_rel(X):-
        X==end_of_file.
add_rel(X):-
    \+ X==end_of_file,
    X = (A,B,D),
    assertz(di(A,B,D)),
    assertz(di(B,A,D)).

add_cj(X):-
        X==end_of_file.
add_cj(X):-
    \+ X==end_of_file,
    X = (A,B),
    assertz(cj(A,B)).

poli(r).
poli(v).
poli(az).
poli(am).
poli(m).

/* posiciones de salida de las pes */
salida_pe(1,3).
salida_pe(6,21).
salida_pe(2,27).
salida_pe(3,65).
salida_pe(4,84).
salida_pe(4,147).
salida_pe(4,149).
salida_pe(4,158).

/* descripcion del etapa */

linternas_por_noche(1,2).
linternas_por_noche(2,2).
linternas_por_noche(3,1).
linternas_por_noche(4,1).

carromatos_por_noche(1,3).
carromatos_por_noche(2,2).
carromatos_por_noche(3,2).
carromatos_por_noche(4,1).
