calc:-
    carga_tablero,
    open('jack_rel.txt',write,ID,[type(text),buffer(false)]),abolish(file_id,1),assertz(file_id(ID)),
    assertz(distancia(0,0,0)),
    foreach(cx(A,B),registra(A,B,1)),
    itera(10,20),
    itera(20,30),
    itera(30,40),
    itera(40,50),
    itera(50,60),
    itera(60,70),
    itera(80,90),
    itera(90,100),
    itera(100,110),
    itera(110,120),
    itera(120,130),
    itera(130,140),
    itera(140,150),
    itera(150,160),
    itera(160,170),
    itera(170,180),
    itera(180,190),
    itera(1,195).

itera(A,B):-
    numlist(A,B,NL),
    numlist(A,B,NL2),
    foreach(member(X,NL),
        foreach((member(Y,NL2),\+X=Y),
                (write(X),write("/"),write(Y),nl,calc_d(X,Y,1)))).

calc_d(X,Y,D):-
    D=<15,
    camino(X,Y,D,[]),!.
calc_d(X,Y,D):-
    D2 is D+1,
    calc_d(X,Y,D2). 

camino(A,B,M,_):-
        M>0,distancia(A,B,M),!.
camino(A,B,M,VIS):-
        M>1,conectados(A,W),\+member(W,VIS),\+ W=B,M2 is (M-1),
        camino(W,B,M2,[A|VIS]),registra(A,B,M).

conectados(A,B):-
        (cx(A,B);cx(B,A)).
registra(X,Y,D):-
        \+(distancia(X,Y,D);distancia(Y,X,D)),
        assertz(distancia(X,Y,D)),
        assertz(distancia(Y,X,D)),
        file_id(ID),
        write(ID,X),write(ID,","),write(ID,Y),write(ID,","),write(ID,D),nl(ID).
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

