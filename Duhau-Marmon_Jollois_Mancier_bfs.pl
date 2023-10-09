%Prédicats dynamiques étant amenés à être modifiés avec assert & retract
:- dynamic board/1.
:- dynamic possible/4.
:- dynamic vu/1.
:- dynamic parent/6.
:- dynamic scoreMini/1.
%Prédicat permettant l'encodage en utf-8 et d'insérer des accents dans les affichages graphiques
:- encoding(utf8).

board(1,[2,2,0,0,0,3,4,0,0,5,0,3,4,1,1,5,0,3,4,0,0,5,0,0,6,0,0,0,7,7,6,0,8,8,8,0]). %Niveau 1 : 16 coups, 1072 noeuds ouverts
board(5,[8,8,9,9,9,6,3,0,0,0,0,6,3,0,0,1,1,6,3,10,10,4,11,11,7,2,0,4,5,0,7,2,12,12,5,0]). %50, 257
board(6,[0,0,2,3,3,3,0,0,2,0,4,0,0,1,1,5,4,0,6,6,7,5,0,8,0,9,7,10,10,8,0,9,11,11,11,0]). %42, 6483
board(2,[2,0,0,3,0,0,2,0,0,3,0,0,2,1,1,3,0,0,0,0,5,4,4,4,0,0,5,0,0,7,0,0,6,6,6,7]). %22, 435
board(4,[0,2,3,3,4,6,0,2,0,5,4,6,0,1,1,5,0,7,0,0,8,8,0,7,0,0,0,9,0,0,0,0,0,9,0,0]). %21, 5212
board(3,[2,0,0,6,6,6,2,3,3,7,0,0,1,1,4,7,0,8,0,0,4,0,0,8,0,0,5,10,10,8,0,0,5,9,9,9]). %18, 2332
board(7,[2,2,3,3,4,0,0,0,9,0,4,5,0,7,9,1,1,5,6,7,0,10,10,5,6,0,0,11,12,12,6,8,8,11,13,13]). %32, 10756
board(8,[4,4,12,0,0,0,0,0,12,0,11,11,3,2,1,1,9,10,3,2,8,8,9,10,0,0,6,0,7,7,5,5,6,0,0,0]). %34, 15129
board(9,[2,3,4,4,5,6,2,3,7,0,5,6,2,0,7,1,1,8,9,9,10,11,0,8,0,0,10,11,12,12,0,13,13,14,14,0]). %71, 4168
board(10,[10,10,10,4,5,6,11,14,14,4,5,6,11,0,1,1,5,6,12,12,3,0,0,0,0,2,3,0,7,7,0,2,8,8,9,9]). %Niveau le plus compliqué : 93 coups, 19480

demarrerconsole() :- board(1,B), retractall(board(_)), asserta(board(B)), grille(_).

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

%Parcourt toutes les cases du board et assert le prédicat possible/4 si un mouvement depuis cette case produit un board qui n'a pas été exploré
possible() :- retractall(possible(_,_,_,_)), possible(2,1).
possible(7,Y) :- Y2 is Y+1, possible(1,Y2), !.
possible(_,7) :- !.
possible(X,Y) :- X2 is X+1, (mouvement(X,Y,Direction,Board,NvBoard) -> (not(vu(NvBoard)) ->
    (asserta(possible(X,Y,Direction,NvBoard)), asserta(vu(NvBoard)), Case is X-1+(Y-1)*6, nth0(Case,Board,Vehicule), asserta(parent(Board,NvBoard,Direction,Vehicule,X,Y)),
    possible(X2,Y)), ! ; possible(X2,Y), !) ; possible(X2,Y), !).
possibles(R) :- possible(), findall(B,possible(_,_,_,B),R).

%Sert à l'affichage, les prédicats vu/1 et parent/6 ne sont utiles que pour l'exploration
possible2() :- retractall(possible(_,_,_,_)), possible2(2,1).
possible2(7,Y) :- Y2 is Y+1, possible2(1,Y2), !.
possible2(_,7) :- !.
possible2(X,Y) :- X2 is X+1, (mouvement(X,Y,Direction,_,NvBoard) -> (asserta(possible(X,Y,Direction,NvBoard)), possible2(X2,Y)), ! ; possible2(X2,Y), !).

%Remplace un élément de la liste
%replace(Liste, Indice, NouvelleValeur, NouvelleListe)
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%Exécuter deux fois replace est plus rapide que swap
/*swap(As,I,J,Cs) :-
    same_length(As,Cs),
    append(BeforeI,[AtI|PastI],As),
    append(BeforeI,[AtJ|PastI],Bs),
    append(BeforeJ,[AtJ|PastJ],Bs),
    append(BeforeJ,[AtI|PastJ],Cs),
    length(BeforeI,I),
    length(BeforeJ,J).*/

%Véhicule cible représentée par le chiffre 1
gagner(L) :- nth0(17,L,N), N==1. %[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]

%IA
%Breadth First Search
bfs() :- board(L), gagner(L), !.
bfs() :- board(L), asserta(vu(L)), possibles(Prochains), bfs(Prochains), !.
bfs([T|_]) :- retractall(board(_)), asserta(board(T)), gagner(T), !.
bfs([T|Q]) :- retractall(board(_)), asserta(board(T)), possibles(Prochains), append(Q,Prochains,Q2), bfs(Q2), !.

%Affichage
nombrecoups(Board, N,D) :- parent(AncienBoard,Board,_,_,_,_) -> N2 is N+1, nombrecoups(AncienBoard,N2,D), !;
    new(G,text('Résolu en\n')),new(H,text(N)), new(I,text(' coups minimum!')), send(G,  font, font(times, bold, 14)),send(H,  font, font(times, bold, 14)),send(I,  font, font(times, bold, 14)),
    send(D, display, G, point(270, 140)), send(D, display, H, point(345, 140)),send(D, display, I, point(360,140)), effacer(), !.

nombrecoupsMini(Board, N,D) :- parent(AncienBoard,Board,_,_,_,_) -> N2 is N+1, nombrecoupsMini(AncienBoard,N2,D), !;
    new(G,text('Niveau résolu en\n')),new(H,text(N)), new(I,text(' coups minimum!')), send(G,  font, font(times, bold, 14)),send(H,  font, font(times, bold, 14)),send(I,  font, font(times, bold, 14)),
    send(D, display, G, point(255, 70)), send(D, display, H, point(370, 70)),send(D, display, I, point(385,70)),retractall(scoreMini(_)), asserta(scoreMini(N)), effacer(), !.

solution(Board, N) :- parent(AncienBoard,Board,Direction,Vehicule,_,_) -> write('\n'), write('Vehicule '), write(Vehicule), write(' ; direction : '), write(Direction),
    N2 is N+1, solution(AncienBoard, N2), !;
    write('\n'), write('Solution en '), write(N), write(' coups (lire de bas en haut).'), write('\n'), effacer().

prochaincoup(Board,D) :- parent(AncienBoard,Board,Direction,_,X,Y) -> (parent(_,AncienBoard,_,_,_,_) -> prochaincoup(AncienBoard,D), !;
    (Direction == gauche -> K is 40 * (X-1) + 16, Z is 40 * (Y-1) + 20, new(I,text('<=',center,normal)), send(I,  font, font(times, bold, 18)), send(D, display, I, point(K,Z)), effacer() ;
    (Direction == droite -> K is 40 * (X-1) + 20, Z is 40 * (Y-1) + 19, new(I,text('=>',center,normal)), send(I,  font, font(times, bold, 18)), send(D, display, I, point(K,Z)), effacer() ;
    (Direction == haut -> K is 40 * (X-1) + 26, Z is 40 * (Y-1) + 15, new(I,text('^\n||',center,normal)), send(I,  font, font(times, bold, 13)), send(D, display, I, point(K,Z)), effacer() ;
    (Direction == bas -> K is 40 * (X-1) + 24, Z is 40 * (Y-1) + 13, new(I,text('||\nV',center,normal)), send(I,  font, font(times, bold, 15)), send(D, display, I, point(K,Z)), effacer() ; ! )))) ; !) ; !.

effacer() :- retractall(vu(_)), retractall(parent(_,_,_,_,_,_)), retractall(dist(_,_)), retractall(disth(_,_)).