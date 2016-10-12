/* AI para el juego de mesa sombras sobre londres 
 (C) Jorge de Antonio , 2016, jordi.deantonio@gmail.com
*/

jack :-
        write("Sombras sobre Londres... v.0.1, (C) Jorge de Antonio,"),nl,
        write("   empieza una nueva partida"),nl,
        init.

elige_guarida :-
        random(1,200,G),
        assert(guarida(G))
        write(" .... Ya tengo mi guarida... bwahahahaha...."),nl.

otra_noche_mas :-
        jack_libre,
        queda_por_matar,
        noche(N),retract(noche(_)),N is N+1,assert(noche(N)).
        retract(pe(_,_)),
        cuantas_pes(N,P),
        coloca_pes_una_a_una(P).
        write(" .... ya he colocado las pe's, comienza noche "),write(N),write(",bwahahahaha...."),nl,
        aun_queda_por_matar(N).

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
        jack_se_mueve_entre_la_niebla(C),
        assert(he_estado(C)),
        avisa_si_en_guarida.
mueve_jack :-
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
        he_estado(C),
        write(".... ssiiii!!!!!"),nl.
pista(_) :- write(" .... mmmmmmmhhh  no!! :D"),nl.


/* Funciones auxiliares .*/
init:-
        retract(guarida(_)),
        retract(poli(_,_,_)),
        retract(crime_scene(_)),
        assert(jack_libre),
        assert(queda_por_matar),
        retract(noche(_)),
        retract(he_estado(_)),
        assert(noche(0)).

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
        assert(pe(P,C)).
        
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
        retract(posicion_jack(_)),assert(posicion_jack(C)).

elige_donde_jack(C,CC,RC) :-
        random(0,1,X),
        decide(X,C,CC,RC).
decide(0,C,_,C).
decide(1,_,CC,CC).

jack_no_en_guarida :- posicion_jack(C),guarida(G),C \== G.
jack_en_guarida :- posicion_jack(C),guarida(C).

poli_esta_en(P,0,0):-.
poli_esta_en(P,A,B):-
        assert(poli(P,A,B)),
        donde_poli(P).
        
avisa_si_en_guarida :-
        jack_en_guarida,write("Llegue a mi guarida, bwaahhahahahaaha"),nl.
avisa_si_en_guarida :-.

aun_queda_por_matar(4) :-
        write("... ganeeeeeeeeeeeee.... bwahahahaha"),nl,
        retract(aun_queda_por_matar).
aun_queda_por_matar(_):-.

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


