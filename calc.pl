calc:-
    init,
    open('jack_rel.txt',append,ID,[type(text),buffer(false)]),abolish(file_id,1),assertz(file_id(ID)),
    itera(1,195).

init:-
    carga_tablero,
    carga_distancias.

itera(A,B):-
    numlist(A,B,NL),
    numlist(A,B,NL2),
    assert(idthA(0)),
    assert(idthB(0)),
    foreach(member(X,NL),
        foreach((member(Y,NL2),\+X=Y,\+distancia(X,Y,_)),
                (write(X),write("/"),write(Y),nl,calc_d(X,Y,1)))).

calc_d(X,Y,D):-
        idthA(0),
        thread_create(calc_dd(X,Y,D),ID,[]),
        retract(idthA(0)),assert(idthA(ID)).
calc_d(X,Y,D):-
        idthB(0),
        thread_create(calc_dd(X,Y,D),ID,[]),
        retract(idthB(0)),assert(idthB(ID)).
calc_d(X,Y,D):-
        idthA(IDA),
        idthB(IDB),
        thread_join(IDA,_),
        retract(idthA(IDA)),assert(idthA(0)),
        calc_d(X,Y,D),
        thread_join(IDB,_),
        retract(idthB(IDB)),assert(idthB(0)),
        calc_d(X,Y,D).

calc_dd(X,Y,D):-
    D=<15,
    \+(dist_seguro_mayor(X,Y,DD),DD>=D),
    camino(X,Y,D,[X]),!.
calc_dd(X,Y,D):-
    D2 is D+1,
    calc_dd(X,Y,D2). 

camino(A,B,M,_):-
        M>0,distancia(A,B,M),!.
camino(A,B,M,VIS):-
        M>1,conectados(A,W),\+member(W,VIS),\+ W=B,M2 is (M-1),
        camino(W,B,M2,[A|VIS]),registra(A,B,M).
camino(A,B,M,_):-
        assert(dist_seguro_mayor(A,B,M)),
        assert(dist_seguro_mayor(B,A,M)),fail.

conectados(A,B):-
        (cx(A,B);cx(B,A)).
registra(X,Y,D):-
        \+(distancia(X,Y,D);distancia(Y,X,D)),
        assertz(distancia(X,Y,D)),
        assertz(distancia(Y,X,D)),
        file_id(ID),
        write(ID,X),write(ID,","),write(ID,Y),write(ID,","),write(ID,D),write(ID,"."),nl(ID).
registra(_,_,_).

add_cx(X):-
        X==end_of_file.
add_cx(X):-
    \+ X==end_of_file,
    X = (A,B),
    assertz(cx(A,B)).

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
    close(IDT).

add_rel(X):-
        X==end_of_file.
add_rel(X):-
    \+ X==end_of_file,
    X = (A,B,D),
    assertz(distancia(A,B,D)),
    assertz(distancia(B,A,D)),
    D2 is D-1,
    assertz(dist_seguro_mayor(A,B,D2)),
    assertz(dist_seguro_mayor(B,A,D2)).
