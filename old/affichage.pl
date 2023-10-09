%Prédicats dynamiques étant amenés à être modifiés avec assert & retract
:- dynamic position/3.
:- dynamic occupation/3.
:- dynamic vehicule/1.
:- dynamic vehicules/1.
:- dynamic longueur/2.
:- dynamic orientation/2.
:- dynamic position/3.
:- dynamic board/1.
:- dynamic score/1.

:- encoding(utf8).



boucle(P,N) :- forall(between(1, N, _), P). %Peut être utile pour répéter une action

board1([2,2,0,0,0,3,4,0,0,5,0,3,4,1,1,5,0,3,4,0,0,5,0,0,6,0,0,0,7,7,6,0,8,8,8,0]). %Niveau 1 %Réfléchir à comment changer les niveaux !
board2([8,8,9,9,9,6,3,0,0,0,0,6,3,0,0,1,1,6,3,10,10,4,11,11,7,2,0,4,5,0,7,2,12,12,5,0]).
board3([10,10,10,4,5,6,11,14,14,4,5,6,11,0,1,1,5,6,12,12,3,0,0,0,0,2,3,0,7,7,0,2,8,8,9,9]). %Niveau le plus compliqué : 93 coups mini
        

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
    retract(position(V,X,Y)), asserta(position(V,X,Y2)), Y5 is Y2+L,retract(occupation(V,X,Y5)), asserta(occupation(V,X,Y2)),affiche_solution().
    %La base du véhicule occupe désormais le vide en haut
bouger(V,bas) :- vehicules(W), orientation(V,verticale), longueur(V,L), position(V,X,Y), Y2 is Y+L, Y2=<6, not(occupee(X,Y2,W)),
    board(B), Y3 is Y-1, Y4 is Y3*6, X2 is X-1, N is X2+Y4, L2 is L*6, N2 is N+L2, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), Y5 is Y+1, asserta(position(V,X,Y5)), retract(occupation(V,X,Y)), asserta(occupation(V,X,Y2)),affiche_solution().
    %Le sommet du véhicule occupe désormais le vide en bas
bouger(V,gauche) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X-1, X2>=1, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X2-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), asserta(position(V,X2,Y)), X4 is X2+L,retract(occupation(V,X4,Y)), asserta(occupation(V,X2,Y)),affiche_solution().

    %L'extrême droite du véhicule occupe désormais le vide à gauche
bouger(V,droite) :- vehicules(W), orientation(V,horizontale), longueur(V,L), position(V,X,Y), X2 is X+L, X2=<6, not(occupee(X2,Y,W)),
    board(B), Y2 is Y-1, Y3 is Y2*6, X3 is X-1, N is X3+Y3, N2 is N+L, swap(B,N,N2,C), retractall(board(_)), asserta(board(C)),
    retract(position(V,X,Y)), X4 is X+1, asserta(position(V,X4,Y)), retract(occupation(V,X,Y)), asserta(occupation(V,X2,Y)),affiche_solution().

%cas de la voiture rouge qui gagne
bougerfin(1,droite,D) :- orientation(1,horizontale),longueur(1,2),position(1,5,3),gagner(D).


    %L'extrême gauche du véhicule occupe désormais le vide à droite

%Echange de deux éléments d'indices I & J d'une liste
swap(L,I,J,L2) :-
   same_length(L,L2), %La longueur des deux listes est égale
   append(AvantI,[EnI|ApresI],L), %Correspondance avec la première liste
   append(AvantI,[EnJ|ApresI],X),
   append(AvantJ,[EnJ|ApresJ],X), %AvantI=AvantJ et ApresI=ApresJ
   append(AvantJ,[EnI|ApresJ],L2), %Correspondance avec la seconde liste
   length(AvantI,I),
   length(AvantJ,J). %La longueur des sous-listes correspond aux indices I & J

demarrer() :- % on crée la fenêtre
	new(F, window('Bienvenue au jeu de Rush Hour')),
	% on lui donne la bonne taille
	send(F, size, size(700, 100)),
	% on crée un composant texte
	new(T, text('Bienvenue au jeu de Rush Hour! Vous avez pour objectif de faire sortir la voiture rouge en \n bougeant les autres voitures qui bougent si on clique sur les côtés')),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, T, point(30, 20)),
    send(F, display, button('Ok, je suis prêt à faire le premier niveau', and(message(@prolog,demarrer,1),message(F,destroy))),point(20,60)),
    send(F, display, button('Je veux faire un niveau assez dur', and(message(@prolog,demarrer,2),message(F,destroy))),point(280,60)),
    send(F, display, button('Je suis une bête à ce jeu !', and(message(@prolog,demarrer,3),message(F,destroy))),point(510,60)),
    send(F, open).
	% on envoie à la fenêtre le message d'affichage.

board([2,2,0,0,0,3,4,0,0,5,0,3,4,1,1,5,0,3,4,0,0,5,0,0,6,0,0,0,7,7,6,0,8,8,8,0]).

score(1).

scoreAug():- score(B),B1 is B+1,asserta(score(B1)).

gagner(D) :- % on crée la fenêtre
    score(X),
	new(F, window('Rush Hour Winner')),
	% on lui donne la bonne taille
	send(F, size, size(650, 100)),
	% on crée un composant texte
	new(T, text('Félicitations! Vous avez réussi à faire sortir la voiture rouge et gagner le niveau de ce jeu!')),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, T, point(80, 20)),
    new(G, text('Vous avez réussi à vous en sortir en : ')),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, G, point(80, 35)),
    new(I, text(X)),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, I, point(300, 35)),
    new(L, text('coups')),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, L, point(320, 35)),
    send(F, display, button(ok, and(message(F, destroy),message(D,destroy))),point(100,60)),
    send(F, display, button('refaire la même partie', and(message(@prolog,demarrer,1),message(F,destroy),message(D,destroy))),point(250,60)),
    send(F, display, button('faire une partie plus dur', and(message(@prolog,demarrer,2),message(F,destroy),message(D,destroy))),point(400,60)),
    send(F, open).
	% on envoie à la fenêtre le message d'affichage.
demarrer(1):-
        retractall(board(_)), board1(B), asserta(board(B)),
        retractall(partieEnCours(_)), asserta(partieEnCours(1)),
        retractall(score(_)), asserta(score(0)),
        grille(init,1,1,B),
        % création de la fenêtre d'affichage avec son titre
        new(D, window('Rush Hour')),
        new(T, text('Nombre de coups :')),
        send(T,  font, font(times, bold, 18)),
	% on demande à la fenêtre de l'afficher à peu près au milieu
        send(D, display, T, point(290, 20)),
        new(G,text(0)),
        send(G,  font, font(times, bold, 18)),
        % on demande à la fenêtre de l'afficher à peu près au milieu
        send(D, display, G, point(360, 50)),
        % calcul des dimensions
        DX is 12 * 40 + 20,
        DY is 6 * 40 + 20,
        % affectation de la taille à la fenêtre
        send(D, size, new(_, size(DX,DY))),
        % affichage de la fenêtre
        send(D, open,point(300, 300)),
        % affichage de la grille proprement dit
        affiche_solution(D, B, 6, 0, 0).

    demarrer(2):-
            retractall(board(_)), board2(B), asserta(board(B)),
            retractall(partieEnCours(_)), asserta(partieEnCours(2)),
            retractall(score(_)), asserta(score(0)),
            grille(init,1,1,B),
            % création de la fenêtre d'affichage avec son titre
            new(D, window('Rush Hour')),
            new(T, text('Nombre de coups :')),
            send(T,  font, font(times, bold, 18)),
        % on demande à la fenêtre de l'afficher à peu près au milieu
            send(D, display, T, point(290, 20)),
            new(G,text(0)),
            send(G,  font, font(times, bold, 18)),
            % on demande à la fenêtre de l'afficher à peu près au milieu
            send(D, display, G, point(360, 50)),
            % calcul des dimensions
            DX is 12 * 40 + 20,
            DY is 6 * 40 + 20,
            % affectation de la taille à la fenêtre
            send(D, size, new(_, size(DX,DY))),
            % affichage de la fenêtre
            send(D, open,point(300, 300)),
            % affichage de la grille proprement dit
            affiche_solution(D, B, 6, 0, 0).

        demarrer(3):-
                retractall(board(_)), board3(B), asserta(board(B)),
                retractall(partieEnCours(_)), asserta(partieEnCours(3)),
                retractall(score(_)), asserta(score(0)),
                grille(init,1,1,B),
                % création de la fenêtre d'affichage avec son titre
                new(D, window('Rush Hour')),
                new(T, text('Nombre de coups :')),
                send(T,  font, font(times, bold, 18)),
            % on demande à la fenêtre de l'afficher à peu près au milieu
                send(D, display, T, point(290, 20)),
                new(G,text(0)),
                send(G,  font, font(times, bold, 18)),
                % on demande à la fenêtre de l'afficher à peu près au milieu
                send(D, display, G, point(360, 50)),
                % calcul des dimensions
                DX is 12 * 40 + 20,
                DY is 6 * 40 + 20,
                % affectation de la taille à la fenêtre
                send(D, size, new(_, size(DX,DY))),
                % affichage de la fenêtre
                send(D, open,point(300, 300)),
                % affichage de la grille proprement dit
                affiche_solution(D, B, 6, 0, 0).


affiche_solution() :-
    board(B),
    grille(init,1,1,B),
	% création de la fenêtre d'affichage avec son titre
    new(D, window('Rush Hour')),
    score(X),
    new(T, text('Nombre de coups :')),
    send(T,  font, font(times, bold, 18)),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(D, display, T, point(290, 20)),
    new(G,text(X)),
    send(G,  font, font(times, bold, 18)),
	% on demande à la fenêtre de l'afficher à peu près au milieu
    send(D, display, G, point(360, 50)),
    send(D, display, button('Abandonner la partie', message(@prolog,perdu,D)),point(310,200)),
	% calcul des dimensions
    DX is 12 * 40 + 20,
	DY is 6 * 40 + 20,
	% affectation de la taille à la fenêtre
	send(D, size, new(_, size(DX,DY))),
	% affichage de la fenêtre
	send(D, open,point(300, 300)),
	% affichage de la grille proprement dit
	affiche_solution(D, B, 6, 0, 0).

perdu(D) :- % on crée la fenêtre
    score(X),
    new(F, window('Rush Hour Loser')),
    % on lui donne la bonne taille
    send(F, size, size(650, 180)),
    % on crée un composant texte
    new(T, text('Vous avez décidé d''abandonner! Vous n''avez pas réussi à faire sortir la voiture rouge ! :-/')),
    % on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, T, point(80, 20)),
    new(G, text('Vous avez réalisé durant ce défi : ')),
    % on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, G, point(80, 35)),
    new(I, text(X),
    % on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, I, point(320, 35)),
    new(L, text('coups')),
    % on demande à la fenêtre de l'afficher à peu près au milieu
    send(F, display, L, point(340, 35)),
    send(F, display, button('Arreter de jouer', and(message(F, destroy),message(D,destroy))),point(100,90)),
    partieEnCours(M),
    (M\=1, send(F, display, button('Retenter de résoudre le même défi', and(message(@prolog,demarrer,M),message(F,destroy),message(D,destroy))),point(250,60));true),
    (M=1, send(F, display, button('Retenter de résoudre le même défi', and(message(@prolog,demarrer,M),message(F,destroy),message(D,destroy))),point(250,90));true),
    ( M \= 3, send(F, display, button('Faire un défi un peu plus dur', and(message(@prolog,demarrer,M+1),message(F,destroy),message(D,destroy))),point(250,120));true),
    ( M = 3, send(F, display, button('Faire le defi N°1 (le plus facile)', and(message(@prolog,demarrer,1),message(F,destroy),message(D,destroy))),point(250,120));true),
    ( M \= 1, send(F, display, button('Faire un défi un peu plus facile', and(message(@prolog,demarrer,M-1),message(F,destroy),message(D,destroy))),point(250,90));true),
    ( M = 1, send(F, display, button('Faire le défi HardCore (ultra-dur)', and(message(@prolog,demarrer,3),message(F,destroy),message(D,destroy))),point(250,150));true),    
    send(F, open).
        % on envoie à la fenêtre le message d'affichage.


%affiche_solution(Fenetre, 
%		Solution, 
%		Largeur, 
%		LigneCourante, 
%		CaseCourante)
% ici il n'y a plus rien à afficher, on s'arrête.
affiche_solution(_D, [], _, _, _) :- !.

% lorsqu'on a terminé d'afficher une ligne

% on affiche la case H
affiche_solution(D, Solution, Largeur, L, Largeur) :-
    L==2,
    % on calcule sa position d'affichage dans la fenêtre
    X is 40 * 6 + 15,
    Y is 40 * 2 + 16,
        % on crée un carré blanc de 40 pixels sur 40
        new(B, text('=> Exit')),
        send(B,  font, font(times, bold, 18)),
        
        % et on dit à la fenêtre de l'afficher
        send(D,display, B, point(X,Y)),
        % on passe à la case suivante
        L1 is L+1,
        affiche_solution(D, Solution, Largeur, L1, 0).

affiche_solution(D, Solution, Largeur, L, Largeur) :-
            % on passe à la ligne suivante.
            L1 is L+1,
            affiche_solution(D, Solution, Largeur, L1, 0).

affiche_solution(D, [H | T], Largeur, L, C) :-
	% on crée un carré blanc de 40 pixels sur 40
	new(B, box(40,40)),
	% on le colorie ou on le laisse intact 
    %new(Image, bitmap('%Mettre le chemin d'un bitmap')),
    ( H = 1,  send(B, fill_pattern,  colour(red)); true),
    ( H = 2,  send(B, fill_pattern,  colour(green)); true),
    ( H = 3,  send(B, fill_pattern,  colour(cyan)); true),
    ( H = 4,  send(B, fill_pattern,  colour(violet)); true),
    ( H = 5,  send(B, fill_pattern,  colour(gold)); true),
    ( H = 6,  send(B, fill_pattern,  colour(brown)); true),
    ( H = 7,  send(B, fill_pattern,  colour(gray)); true),
    ( H = 8,  send(B, fill_pattern,  colour(pink));true),
    ( H = 9,  send(B, fill_pattern,  colour(purple)); true),
    ( H = 10,  send(B, fill_pattern,  colour(yellow)); true),
    ( H = 11,  send(B, fill_pattern,  colour(blue)); true),
    ( H = 12,  send(B, fill_pattern,  colour(orange)); true),
    ( H = 13,  send(B, fill_pattern,  colour(cyan)); true),
    ( H = 14,  send(B, fill_pattern,  colour(green)); true),
    ( H = 15,  send(B, fill_pattern,  colour(black)); true),
    click(H,L,C,B,D),
	% on calcule sa position d'affichage dans la fenêtre
	X is 40 * C + 10,
	Y is 40 * L + 10,
	% et on dit à la fenêtre de l'afficher
	send(D, display, B, point(X,Y)),
	% on passe à la case suivante
	C1 is C+1,
	affiche_solution(D, T, Largeur, L, C1).

click(H,L,C,B,D):- orientation(H,verticale),C1 is C+1,
        L1 is L+1, position(H,C1,L1),!,
        send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,haut),message(D,destroy)))).

click(H,L,C,B,D):- orientation(H,verticale),longueur(H,2),C1 is C+1, position(H,C1,L),!,
            send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,bas),message(D,destroy)))).

click(H,L,C,B,D):- orientation(H,verticale), longueur(H,3),C1 is C+1,L1 is L-1, position(H,C1,L1),!,
            send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,bas),message(D,destroy)))).

click(H,L,C,B,D):- orientation(H,horizontale),C1 is C+1,
        L1 is L+1, position(H,C1,L1),!,
            send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,gauche),message(D,destroy)))).

click(1,2,5,B,D):- orientation(1,horizontale), longueur(1,2), position(1,5,3),!,
            send(B, recogniser,click_gesture(left,'',single,message(@prolog,bougerfin,1,droite,D))).
        
click(H,L,C,B,D):- orientation(H,horizontale), longueur(H,2),L1 is L+1, position(H,C,L1),!,
            send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,droite),message(D,destroy)))).
        
click(H,L,C,B,D):- orientation(H,horizontale), longueur(H,3),C1 is C-1,L1 is L+1, position(H,C1,L1),!,
            send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,H,droite),message(D,destroy)))).
click(_,_,_,_,_):- !.