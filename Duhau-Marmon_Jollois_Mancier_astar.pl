%Prédicats dynamiques étant amenés à être modifiés avec assert & retract
:- dynamic board/1.
:- dynamic possible/4.
:- dynamic vu/1.
:- dynamic parent/4.
:- dynamic disth/2.
:- dynamic dist/2.

board(1,[2,2,0,0,0,3,4,0,0,5,0,3,4,1,1,5,0,3,4,0,0,5,0,0,6,0,0,0,7,7,6,0,8,8,8,0]). %Niveau 1 : 16 coups
board(5,[8,8,9,9,9,6,3,0,0,0,0,6,3,0,0,1,1,6,3,10,10,4,11,11,7,2,0,4,5,0,7,2,12,12,5,0]). %50
board(6,[0,0,2,3,3,3,0,0,2,0,4,0,0,1,1,5,4,0,6,6,7,5,0,8,0,9,7,10,10,8,0,9,11,11,11,0]). %42
board(2,[2,0,0,3,0,0,2,0,0,3,0,0,2,1,1,3,0,0,0,0,5,4,4,4,0,0,5,0,0,7,0,0,6,6,6,7]). %22
board(4,[0,2,3,3,4,6,0,2,0,5,4,6,0,1,1,5,0,7,0,0,8,8,0,7,0,0,0,9,0,0,0,0,0,9,0,0]). %21
board(3,[2,0,0,6,6,6,2,3,3,7,0,0,1,1,4,7,0,8,0,0,4,0,0,8,0,0,5,10,10,8,0,0,5,9,9,9]). %18
board(7,[2,2,3,3,4,0,0,0,9,0,4,5,0,7,9,1,1,5,6,7,0,10,10,5,6,0,0,11,12,12,6,8,8,11,13,13]). %32
board(8,[4,4,12,0,0,0,0,0,12,0,11,11,3,2,1,1,9,10,3,2,8,8,9,10,0,0,6,0,7,7,5,5,6,0,0,0]). %34
board(9,[2,3,4,4,5,6,2,3,7,0,5,6,2,0,7,1,1,8,9,9,10,11,0,8,0,0,10,11,12,12,0,13,13,14,14,0]). %71
board(10,[10,10,10,4,5,6,11,14,14,4,5,6,11,0,1,1,5,6,12,12,3,0,0,0,0,2,3,0,7,7,0,2,8,8,9,9]). %Niveau le plus compliqué : 93 coups

demarrer() :- board(1,B), retractall(board(_)), asserta(board(B)), grille(_). %Choix niveau

%Fabrication de la grille à partir de la liste
%Les zéros deviennent des points pour plus de lisibilité
grille([0|Q]) :- board([0|Q]), write('================'), nl, write('||'), write('.'), grille(2,1,Q), !. %Début
grille([T|Q]) :- board([T|Q]), write('================'), nl, write('||'), write(T), grille(2,1,Q), !.
grille(7,6,_) :- write('||'), nl, write('================'), !. %Fin
grille(7,3,[0|Q]) :- write('  EXIT'),nl, write('|'), grille(1,4,[0|Q]), !. %Pas de mur à la sortie
grille(7,3,[T|Q]) :- write('  EXIT'),nl, write('|'), grille(1,4,[T|Q]), !.
grille(7,Y,[0|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(1,Y2,[0|Q]), !. %Changement de ligne
grille(7,Y,[T|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(1,Y2,[T|Q]), !.
grille(X,Y,[0|Q]) :- write('|'), write('.'), X2 is X+1, grille(X2,Y,Q), !. %Prochaine case
grille(X,Y,[T|Q]) :- write('|'), write(T), X2 is X+1, grille(X2,Y,Q), !. %Nombre différent de 0 : c'est un véhicule et il occupe la case (X,Y)

%Bouger implique que la case cible soit vide (==0) et que le mouvement soit initié par une véhicule (\==0)
%Il faut de plus déterminer si le véhicule possède la bonne orientation et si c'est un camion ou une voiture, pour cela on utilise la fontion longueur
%On remplace enfin 2 valeurs dans le board courant pour obtenir le nouveau board
mouvement(X,Y,Direction,Board,NvBoard) :- Y>1, Y<6, board(Board), Chaut is X-1+(Y-2)*6, nth0(Chaut,Board,Vide), Vide==0,
    Case is Chaut+6, nth0(Case,Board,Vehicule), Vehicule\==0,
    (Y<5 -> (longueur(Vehicule,Board,Case,haut3,Cbasbas) -> replace(Board,Chaut,Vehicule,TempBoard), replace(TempBoard,Cbasbas,0,NvBoard), Direction = 'haut', ! ;
    (longueur(Vehicule,Board,Case,haut2,Cbas) -> replace(Board,Chaut,Vehicule,TempBoard), replace(TempBoard,Cbas,0,NvBoard), Direction = 'haut', !) ; false) ;
    (longueur(Vehicule,Board,Case,haut2,Cbas) -> replace(Board,Chaut,Vehicule,TempBoard), replace(TempBoard,Cbas,0,NvBoard), Direction = 'haut', !) ; false).

mouvement(X,Y,Direction,Board,NvBoard) :- Y<6, Y>1, board(Board), Cbas is X-1+Y*6, nth0(Cbas,Board,Vide), Vide==0,
    Case is Cbas-6, nth0(Case,Board,Vehicule), Vehicule\==0,
    (Y>2 -> (longueur(Vehicule,Board,Case,bas3,Chauthaut) -> replace(Board,Cbas,Vehicule,TempBoard), replace(TempBoard,Chauthaut,0,NvBoard), Direction = 'bas', ! ;
    (longueur(Vehicule,Board,Case,bas2,Chaut) -> replace(Board,Cbas,Vehicule,TempBoard), replace(TempBoard,Chaut,0,NvBoard), Direction = 'bas', !) ; false) ;
    (longueur(Vehicule,Board,Case,bas2,Chaut) -> replace(Board,Cbas,Vehicule,TempBoard), replace(TempBoard,Chaut,0,NvBoard), Direction = 'bas', !) ; false).

mouvement(X,Y,Direction,Board,NvBoard) :- X>1, X<6, board(Board), Cgauche is X-2+(Y-1)*6, nth0(Cgauche,Board,Vide), Vide==0,
    Case is Cgauche+1, nth0(Case,Board,Vehicule), Vehicule\==0,
    (X<5 -> (longueur(Vehicule,Board,Case,gauche3,Cdroitedroite) -> replace(Board,Cgauche,Vehicule,TempBoard), replace(TempBoard,Cdroitedroite,0,NvBoard), Direction = 'gauche', ! ;
    (longueur(Vehicule,Board,Case,gauche2,Cdroite) -> replace(Board,Cgauche,Vehicule,TempBoard), replace(TempBoard,Cdroite,0,NvBoard), Direction = 'gauche', !) ; false) ;
    (longueur(Vehicule,Board,Case,gauche2,Cdroite) -> replace(Board,Cgauche,Vehicule,TempBoard), replace(TempBoard,Cdroite,0,NvBoard), Direction = 'gauche', !) ; false).

mouvement(X,Y,Direction,Board,NvBoard) :- X<6, X>1, board(Board), Cdroite is X+(Y-1)*6, nth0(Cdroite,Board,Vide), Vide==0,
    Case is Cdroite-1, nth0(Case,Board,Vehicule), Vehicule\==0,
    (X>2 -> (longueur(Vehicule,Board,Case,droite3,Cgauchegauche) -> replace(Board,Cdroite,Vehicule,TempBoard), replace(TempBoard,Cgauchegauche,0,NvBoard), Direction = 'droite', ! ;
    (longueur(Vehicule,Board,Case,droite2,Cgauche) -> replace(Board,Cdroite,Vehicule,TempBoard), replace(TempBoard,Cgauche,0,NvBoard), Direction = 'droite', !) ; false) ;
    (longueur(Vehicule,Board,Case,droite2,Cgauche) -> replace(Board,Cdroite,Vehicule,TempBoard), replace(TempBoard,Cgauche,0,NvBoard), Direction = 'droite', !) ; false).

%La case cible est-elle occupée par le même véhicule que la case de référence ?
longueur(Vehicule,Board,Case,haut3,Cbasbas) :- Cbasbas is Case+12, nth0(Cbasbas,Board,Vehicule).
longueur(Vehicule,Board,Case,haut2,Cbas) :- Cbas is Case+6, nth0(Cbas,Board,Vehicule).
longueur(Vehicule,Board,Case,bas3,Chauthaut) :- Chauthaut is Case-12, nth0(Chauthaut,Board,Vehicule).
longueur(Vehicule,Board,Case,bas2,Chaut) :- Chaut is Case-6, nth0(Chaut,Board,Vehicule).
longueur(Vehicule,Board,Case,gauche3,Cdroitedroite) :- Cdroitedroite is Case+2, nth0(Cdroitedroite,Board,Vehicule).
longueur(Vehicule,Board,Case,gauche2,Cdroite) :- Cdroite is Case+1, nth0(Cdroite,Board,Vehicule).
longueur(Vehicule,Board,Case,droite3,Cgauchegauche) :- Cgauchegauche is Case-2, nth0(Cgauchegauche,Board,Vehicule).
longueur(Vehicule,Board,Case,droite2,Cgauche) :- Cgauche is Case-1, nth0(Cgauche,Board,Vehicule).

%Remplace un élément de la liste
%replace(Liste, Indice, NouvelleValeur, NouvelleListe)
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%Parcourt toutes les cases du board et assert le prédicat possible/4 si un mouvement depuis cette case produit un board qui n'a pas été exploré jusque là
possible() :- retractall(possible(_,_,_,_)), possible(2,1).
possible(7,Y) :- Y2 is Y+1, possible(1,Y2), !.
possible(_,7) :- !.
possible(X,Y) :- X2 is X+1, (mouvement(X,Y,Direction,Board,NvBoard) -> (not(vu(NvBoard)) ->
    (asserta(possible(X,Y,Direction,NvBoard)), asserta(vu(NvBoard)), Case is X-1+(Y-1)*6, nth0(Case,Board,Vehicule), asserta(parent(Board,NvBoard,Direction,Vehicule)), distheuristique(NvBoard),
    possible(X2,Y)), ! ; possible(X2,Y), !) ; possible(X2,Y), !).
possibles(R2) :- possible(), findall(B,possible(_,_,_,B),R), quicksort(R,R2).
    %(R = [] -> R3 is 1000, ! ; indexmin(R,M,I), nth0(0,R,E), replace(R,I,E,R2), replace(R2,0,M,R3), !).
    %Essai avec indexmin non concluant

%Distance heuristique à la position gagnante
distheuristique(Board) :- distheuristique1(Board,0,17).
distheuristique1(Board,N,C) :- nth0(C,Board,V), V==1 -> D is C+1, distheuristique2(Board,N,D) ; N2 is N+1, C2 is C-1, distheuristique1(Board,N2,C2).
distheuristique2(Board,N,D) :- D>17 -> distance(Board,N) ; nth0(D,Board,V), D2 is D+1, (V\==0 -> N2 is N+1, distheuristique2(Board,N2,D2) ; distheuristique2(Board,N,D2)).

%Distance totale estimée en récupérant le coût jusque là et en l'additionnant à la distance heuristique
distance(Board,H) :- parent(AncienBoard,Board,_,_), dist(AncienBoard,N), N2 is N+1, asserta(dist(Board,N2)), N3 is N2+H, asserta(disth(Board,N3)).

%Quicksort : tri par distance à l'objectif dans une liste de boards
pivot(_, [], [], []).
pivot(Pivot, [Head|Tail], [Head|LessOrEqualThan], GreaterThan) :- disth(Pivot,N), disth(Head,M), N>=M, pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).
pivot(Pivot, [Head|Tail], LessOrEqualThan, [Head|GreaterThan]) :- pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).

quicksort([], []).
quicksort([Head|Tail], Sorted) :- pivot(Head, Tail, List1, List2), quicksort(List1, SortedList1), quicksort(List2, SortedList2), append(SortedList1, [Head|SortedList2], Sorted).

%Fusion de deux listes de boards triées par distance estimée à l'objectif
fus([], A, A) :- !.
fus(A, [], A) :- !.
fus([A|D],[B|E],[C|F]) :- disth(A,N), disth(B,M), (N=<M -> C=A, fus(D, [B|E], F) ; C=B, fus([A|D], E, F)).

/*%Détermine l'index de la position possédant la distance minimale dans une liste de coups possibles
indexmin([X|Xs],Min,Index):-
    indexmin(Xs,X,0,0,Min,Index).

indexmin([],OldMin,OldIndex,_, OldMin, OldIndex).

indexmin([X|Xs],OldMin,_,CurrentIndex, Min, Index):-
    disth(X,N), disth(OldMin,M), N < M,
    NewCurrentIndex is CurrentIndex + 1,
    NewIndex is NewCurrentIndex,
    indexmin(Xs, X, NewIndex, NewCurrentIndex, Min, Index).
indexmin([X|Xs],OldMin,OldIndex,CurrentIndex, Min, Index):-
    disth(X,N), disth(OldMin,M),
    N >= M,
    NewCurrentIndex is CurrentIndex + 1,
    indexmin(Xs, OldMin, OldIndex, NewCurrentIndex, Min, Index).*/

%Véhicule cible représentée par le chiffre 1
gagner(L) :- nth0(17,L,N), N==1. %[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]

%IA
%A*
astar() :- board(L), asserta(dist(L,0)), gagner(L), !.
astar() :- board(L), asserta(vu(L)), possibles(Prochains), astar(Prochains), !.
astar([T|_]) :- retractall(board(_)), asserta(board(T)), gagner(T), chemin(T,0), !.
astar([T|Q]) :- retractall(board(_)), asserta(board(T)), possibles(Prochains),
    fus(Q,Prochains,Q2), astar(Q2), !.
    %(Prochains = 1000 -> astar(Q), ! ;
    %nth0(0,Prochains,M1), indexmin(Q,M2,I),
    %disth(M1,M3), disth(M2,M4), (M3<M4 -> append(Prochains,Q,Q2), astar(Q2), ! ; nth0(0,Q,E), replace(Q,I,E,Q2), replace(Q2,0,M2,Q3), append(Q3,Prochains,Q4), astar(Q4), !)).
    %Essai avec indexmin non concluant

chemin(Board, N) :- (parent(AncienBoard,Board,Direction,Vehicule) ->
    write('Vehicule '), write(Vehicule), write(' ; direction : '), write(Direction), write('\n'), N2 is N+1, chemin(AncienBoard,N2), ! ;
    write('Solution en '), write(N), write(' coups (lire de bas en haut).')).