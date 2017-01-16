calc:-
    carga_tablero,
    open('jack_rel.txt',write,ID,[type(text),buffer(false)]),abolish(file_id,1),assertz(file_id(ID)),
    assertz(distancia(0,0,0)),
    numlist(1,100,NL),
    numlist(101,195,NL2),
    foreach(member(X,NL),
        foreach((member(Y,NL2),\+X=Y),
                (write(X),write("/"),write(Y),nl,calc_d(X,Y,1)))).

calc_d(X,Y,D):-
    D<16,
    camino(X,Y,D,[]),!.
calc_d(X,Y,D):-
    D2 is D+1,
    calc_d(X,Y,D2). 

camino(A,B,1,_):-
        conectados(A,B),!.
camino(A,B,M,_):-
        M>1,se_sabe(A,B,M),!.
camino(A,B,M,VIS):-
        M>1,conectados(A,W),\+member(W,VIS),\+ W=B,M2 is (M-1),
        camino(W,B,M2,[A|VIS]),registra(A,B,M).

conectados(A,B):-
        (cx(A,B);cx(B,A)).

se_sabe(A,B,M):-(distancia(A,B,M);distancia(B,A,M)).

registra(X,Y,D):-
        \+se_sabe(X,Y,D),
        assertz(distancia(X,Y,D)),
        file_id(ID),
        write(ID,X),write(ID,","),write(ID,Y),write(ID,","),write(ID,D),nl(ID).

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

