%Prédicats dynamiques étant amenés à être modifiés avec assert & retract
:- dynamic position/3.
:- dynamic occupation/3.
:- dynamic vehicule/1.
:- dynamic vehicules/1.
:- dynamic longueur/2.
:- dynamic orientation/2.
:- dynamic position/3.
:- dynamic board/1.

boucle(P,N) :- forall(between(1, N, _), P). %Peut être utile pour répéter une action

demarrer() :- board(B), grille(init,1,1,B).

board([2,2,0,0,0,3,4,0,0,5,0,3,4,1,1,5,0,3,4,0,0,5,0,0,6,0,0,0,7,7,6,0,8,8,8,0]). %Niveau 1 %Réfléchir à comment changer les niveaux !
%board[2,2,2,3,4,5,6,7,7,2,3,4,5,0,1,1,3,4,8,8,9,0,0,0,0,a,9,0,b,b,0,a,c,c,d,d] %Niveau le plus compliqué : 93 coups mini


%Ecrase tous les prédicats concernant les véhicules
effacer() :- retractall(vehicule(_)), retractall(vehicules(_)), retractall(occupation(_,_,_)), retractall(position(_,_,_)), retractall(longueur(_,_)), retractall(orientation(_,_)).

%Fabrication de la grille à partir de la liste
%Les zéros deviennent des points pour plus de lisibilité
grille(1,1,[0|Q]) :- write('================'), nl, write('||'), write('.'), grille(2,1,Q), !. %Début
grille(1,1,[T|Q]) :- write('================'), nl, write('||'), write(T), grille(2,1,Q), !.
grille(7,6,_) :- write('||'), nl, write('================'), !. %Fin
grille(7,3,[0|Q]) :- write('  EXIT'),nl, write('|'), grille(1,4,[0|Q]), !. %Pas de mur à la sortie
grille(7,3,[T|Q]) :- write('  EXIT'),nl, write('|'), grille(1,4,[T|Q]), !.
grille(7,Y,[0|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(1,Y2,[0|Q]), !. %Changement de ligne
grille(7,Y,[T|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(1,Y2,[T|Q]), !.
grille(X,Y,[0|Q]) :- write('|'), write('.'), X2 is X+1, grille(X2,Y,Q), !. %Prochaine case
grille(X,Y,[T|Q]) :- write('|'), write(T), X2 is X+1, grille(X2,Y,Q), !. %Nombre différent de 0 : c'est un véhicule et il occupe la case (X,Y)
%init met à jour les prédicats concernant les véhicules
grille(init,1,1,[0|Q]) :- effacer(), write('================'), nl, write('||'), write('.'), grille(init,2,1,Q), !.
grille(init,1,1,[T|Q]) :- effacer(), asserta(vehicule(T)), asserta(occupation(T,1,1)), write('================'), nl, write('||'), write(T), grille(init,2,1,Q), !.
grille(init,7,6,_) :- write('||'), nl, write('================'), vehicules(), !.
grille(init,7,3,[0|Q]) :- nl, write('|'), grille(init,1,4,[0|Q]), !.
grille(init,7,3,[T|Q]) :- nl, write('|'), grille(init,1,4,[T|Q]), !.
grille(init,7,Y,[0|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(init,1,Y2,[0|Q]), !.
grille(init,7,Y,[T|Q]) :- write('||'), nl, write('|'), Y2 is Y+1, grille(init,1,Y2,[T|Q]), !.
grille(init,X,Y,[0|Q]) :- write('|'), write('.'), X2 is X+1, grille(init,X2,Y,Q), !.
grille(init,X,Y,[T|Q]) :- asserta(vehicule(T)), asserta(occupation(T,X,Y)), write('|'), write(T), X2 is X+1, grille(init,X2,Y,Q), !.

grille(init2,1,1,[0|Q]) :- effacer(), grille(init2,2,1,Q), !.
grille(init2,1,1,[T|Q]) :- effacer(), asserta(vehicule(T)), asserta(occupation(T,1,1)), grille(init2,2,1,Q), !.
grille(init2,7,6,_) :- vehicules(), !.
grille(init2,7,3,[0|Q]) :- grille(init2,1,4,[0|Q]), !.
grille(init2,7,3,[T|Q]) :- grille(init2,1,4,[T|Q]), !.
grille(init2,7,Y,[0|Q]) :- Y2 is Y+1, grille(init2,1,Y2,[0|Q]), !.
grille(init2,7,Y,[T|Q]) :- Y2 is Y+1, grille(init2,1,Y2,[T|Q]), !.
grille(init2,X,Y,[0|Q]) :- X2 is X+1, grille(init2,X2,Y,Q), !.
grille(init2,X,Y,[T|Q]) :- asserta(vehicule(T)), asserta(occupation(T,X,Y)), X2 is X+1, grille(init2,X2,Y,Q), !.

%Longueur d'un véhicule = nombre de prédicats associés à ce véhicule
longueurs([]) :- !.
longueurs([T|Q]) :- aggregate_all(count, vehicule(T), L), asserta(longueur(T,L)), longueurs(Q).

%Orientation horizontale si un véhicule apparaît 2 fois à la suite dans la liste du board, sinon orientation verticale
%La position du véhicule est la première case occupée par la voiture sur le board
orientations([],_,_) :- !.
orientations([U|R],[T|Q],N) :- U\==T, N2 is N+1, orientations([U|R],Q,N2), !.
orientations([U|R],[U|Q],N) :- X is N mod 6, X2 is X+1, Y is N//6, Y2 is Y+1, asserta(position(U,X2,Y2)), orientations([U|R],Q), !.
orientations([U|R],[T|_]) :- U\==T, asserta(orientation(U,verticale)), board(B), orientations(R,B,0), !.
orientations([U|R],[U|_]) :- asserta(orientation(U,horizontale)), board(B), orientations(R,B,0), !.

%Ordonner la liste des véhicules en retirant les doublons, lancement du calcul des longueurs et des orientations avec cette liste ordonnée
vehicules() :- setof(X,vehicule(X),L), asserta(vehicules(L)), longueurs(L), board(B), orientations(L,B,0).

%La case est occupée par un des véhicules
occupee(X,Y,[V|_]) :- occupation(V,X,Y), !.
occupee(X,Y,[_|V2]) :- occupee(X,Y,V2).

%Bouger implique la bonne orientation du véhicule, de rester dans le board et de ne pas foncer dans un autre véhicule
bouger(V,haut) :- vehicules(W), orientation(V,verticale), position(V,X,Y), longueur(V,L), Y2 is Y-1, Y2>=1, not(occupee(X,Y2,W)),
    board(B), Y3 is Y2-1, Y4 is Y3*6, X2 is X-1, N is X2+Y4, L2 is L*6, N2 is N+L2, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), asserta(position(V,X,Y2)), Y5 is Y2+L,retract(occupation(V,X,Y5)), asserta(occupation(V,X,Y2)).
    %La base du véhicule occupe désormais le vide en haut
bouger(V,bas) :- vehicules(W), orientation(V,verticale), longueur(V,L), position(V,X,Y), Y2 is Y+L, Y2=<6, not(occupee(X,Y2,W)),
    board(B), Y3 is Y-1, Y4 is Y3*6, X2 is X-1, N is X2+Y4, L2 is L*6, N2 is N+L2, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), Y5 is Y+1, asserta(position(V,X,Y5)), retract(occupation(V,X,Y)), asserta(occupation(V,X,Y2)).
    %Le sommet du véhicule occupe désormais le vide en bas
bouger(V,gauche) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X-1, X2>=1, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X2-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), asserta(position(V,X2,Y)), X4 is X2+L,retract(occupation(V,X4,Y)), asserta(occupation(V,X2,Y)).
    %L'extrême droite du véhicule occupe désormais le vide à gauche
bouger(V,droite) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X+L, X2=<6, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), X4 is X+1, asserta(position(V,X4,Y)), retract(occupation(V,X,Y)), asserta(occupation(V,X2,Y)).
    %L'extrême gauche du véhicule occupe désormais le vide à droite

mouvement(C,V,haut) :- vehicules(W), orientation(V,verticale), position(V,X,Y), longueur(V,L), Y2 is Y-1, Y2>=1, not(occupee(X,Y2,W)),
    board(B), Y3 is Y2-1, Y4 is Y3*6, X2 is X-1, N is X2+Y4, L2 is L*6, N2 is N+L2, swap(B,N,N2,C).

mouvement(C,V,bas) :- vehicules(W), orientation(V,verticale), longueur(V,L), position(V,X,Y), Y2 is Y+L, Y2=<6, not(occupee(X,Y2,W)),
    board(B), Y3 is Y-1, Y4 is Y3*6, X2 is X-1, N is X2+Y4, L2 is L*6, N2 is N+L2, swap(B,N,N2,C).

mouvement(C,V,gauche) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X-1, X2>=1, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X2-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C).

mouvement(C,V,droite) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X+L, X2=<6, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C).

possible(C) :- mouvement(C,_,_).

possibles(B,R) :- board(B), findall(C,mouvement(C,_,_),R).

%Echange de deux éléments d'indices I & J d'une liste
swap(L,I,J,L2) :-
    same_length(L,L2), %La longueur des deux listes est égale
    append(AvantI,[EnI|ApresI],L), %Correspondance avec la première liste
    append(AvantI,[EnJ|ApresI],X),
    append(AvantJ,[EnJ|ApresJ],X), %AvantI=AvantJ et ApresI=ApresJ
    append(AvantJ,[EnI|ApresJ],L2), %Correspondance avec la seconde liste
    length(AvantI,I),
    length(AvantJ,J). %La longueur des sous-listes correspond aux indices I & J

%Véhicule cible représentée par le chiffre 1
gagner(L) :- nth0(17,L,N), N==1.
    %[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]

%IA
state_record(State, Parent, [State, Parent]).

%bfs(L,Vus,Prochains)

bfs(L) :- gagner(L), !.
bfs(L) :- possibles(L,Prochains), bfs(L,Prochains,[]).
bfs(_,[T|_],_) :- gagner(T), !.
bfs(L,[T|Q],Vus) :- asserta(board(T)), grille(init2,1,1,T), append(T,Vus,Vus2), possibles(T,Prochains), remove_list(Prochains, Vus, Prochains2), append(Q,Prochains2,Q2), bfs(L,Q2,Vus2), !.
bfs(_,[],_) :- false.

dfs(L) :- gagner(L), !.
dfs(L) :- possible(L2), retract(board(L)), asserta(board(L2)), grille(init2,1,1,L2), dfs(L2), !.

%A commenter !
remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result).
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).