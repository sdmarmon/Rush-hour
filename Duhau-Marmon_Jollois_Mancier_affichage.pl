%NB : L’affichage est optimisé pour un MacBook Air.

%Prédicat permettant d'insérer l'intelligence artificielle et les règles de jeu dans l'affichage
:- consult('Duhau-Marmon_Jollois_Mancier_bfs').
%Prédicats dynamiques étant amenés à être modifiés avec assert & retract
:- dynamic score/1.
:- dynamic partieEnCours/1.
%Prédicat permettant l'encodage en utf-8 et d'insérer des accents dans les affichages graphiques
:- encoding(utf8).

%Le mouvement est effectué, le board, actualisé
bouger(X,Y) :- mouvement(X,Y,_,_,NvBoard), retractall(board(_)), asserta(board(NvBoard)), possible2(), affiche_solution().

demarrer() :- 
    % on crée la fenêtre de bienvenue 
	new(F, window('Bienvenue au jeu de Rush Hour')),
	% on lui donne la bonne taille
	send(F, size, size(550, 150)),
	% on crée un composant texte explicitant le jeu 
	new(T, text('Bienvenue au jeu de Rush Hour! Vous avez pour objectif de faire sortir la voiture rouge en 
            bougeant les autres voitures qui bougent si on clique sur leurs côtés')),
	% on demande à la fenêtre de le placer à peu près au centre 
    send(F, display, T, point(30, 20)),
    % on crée un slider permettant de sélectionner le niveau qui nous convient et on l'affiche 
    send(F, display, new(Niveau, slider(niveau, 1, 10,1)), point(150,65)),
    %On crée et place dans la fenêtre un bouton permettant de lancer une partie en appelant 
    %le prédicat demarrer avec le niveau en paramètre et on supprime la fenêtre de bienvenue
    send(F, display, button('Démarrer la partie !', and(message(@prolog,demarrer,Niveau?selection),message(F,destroy))),point(250,100)),
    %On affiche la fenêtre à l'utilisateur 
    send(F, open).

%prédicat score initial, permettant l'initialisation à chaque début de partie
score(0).

%score actualisé, qui augmente de 1 à chaque coup
scoreAug():- score(B),B1 is B+1,asserta(score(B1)).

%le prédicat demarrer avec le niveau en paramètre permet de commencer une partie au niveau mis en paramètres
demarrer(X):-
    %Vider le prédicat board et le remplir avec les valeurs initials du niveau choisi
    retractall(board(_)), board(X,B), asserta(board(B)), possible2(),
    %Vider le prédicat partieEnCours et le remplir avec le numero de niveau choisi
    retractall(partieEnCours(_)), asserta(partieEnCours(X)),
    %Initialisation du score à 0
    retractall(score(_)), asserta(score(0)),
    %On affiche la grille de jeu dans la console 
    grille(_),
    %On crée la fenêtre de jeu où on l'on retrouvera la grille de jeu 
    new(D, window('Rush Hour')),
    %On appelle l'intelligence Artificielle bfs (parcours d'arbre en largeur) 
    %afin de nous afficher le nombre de coups minimums à réaliser pour ce niveau
    board(B),
    bfs(),
    board(B2),
    nombrecoupsMini(B2,0,D),
    retractall(board(_)), asserta(board(B)),
    new(T, text('Nombre de coups :')),
    %On met le texte en police Times, gras et de taille 18
    send(T,  font, font(times, bold, 18)),
    send(D, display, T, point(290, 20)),
    new(G,text(0)),
    send(G,  font, font(times, bold, 18)),
    send(D, display, G, point(360, 50)),
    %On crée et place dans la fenêtre un bouton permettant d'abandonner la partie en appelant 
    %le prédicat perdu avec la fenêtre de jeu en paramètre
    send(D, display, button('Abandonner', message(@prolog,perdu,D)),point(267,220)),
    %On crée et place dans la fenêtre un bouton permettant de donner un indice dans la configuration actuelle en appelant 
    %le prédicat pcoup avec la fenêtre de jeu en paramètre
    send(D, display, button('Indice pour le prochain coup', message(@prolog,pcoup,D)),point(320,170)),
    %On crée et place dans la fenêtre un bouton permettant d'afficher la solution entière avec tous les coups 
    %pour résoudre la configuration en appelant le prédicat sol
    send(D, display, button('Solution (console)', message(@prolog,sol)),point(365,220)),
    % calcul des dimensions de la fenêtre de jeu
    DX is 12 * 40 + 20,
    DY is 6 * 40 + 20,
    % affectation de la taille à la fenêtre de jeu
    send(D, size, new(_, size(DX,DY))),
    % affichage de la fenêtre de jeu 
    send(D, open,point(300, 300)),
    % affichage de la grille de jeu proprement dit
    affiche_solution(D, B, 6, 0, 0).

%Prédicat permettant d'appeller nombrecoups dans le fichier rush_hour.pl qui lancera l'intelligence artificielle bfs et affichera le 
%nombre de coups minimum pour résoudre la configuration actuelle des voitures du jeu
mini(D):-
    board(B),
    bfs(),
    board(B2),
    nombrecoups(B2,0,D),
    retractall(board(_)), asserta(board(B)).

%Prédicat permettant d'appeller pcoup dans le fichier rush_hour.pl qui lancera l'intelligence artificielle bfs et affichera l'indice dans
%le jeu dans le but de résoudre la configuration actuelle en un nombre de coups minimum
pcoup(D):-
    board(B),
    bfs(),
    board(B2),
    prochaincoup(B2,D),
    retractall(board(_)), asserta(board(B)).

%Prédicat permettant d'appeller solution dans le fichier rush_hour.pl qui lancera l'intelligence artificielle bfs et affichera la solution
%dans la console en un nombre de coups minimum
sol() :- board(B), bfs(), board(B2), solution(B2,0), retractall(board(_)), asserta(board(B)).

affiche_solution() :-
    board(B),
    grille(_),
    %On crée la fenêtre où l'on retrouvera la grille de jeu
    new(D, window('Rush Hour')),
    % on appelle le prédicat score, pour le récupérer
    score(X),
    % on appelle le prédicat scoreMini, pour récupérer le nombre de coups minimums possibles dans ce niveau
    scoreMini(Y),
    new(G,text('Niveau résolu en')),new(H,text(Y)), new(I,text(' coups minimum!')), 
    send(G,  font, font(times, bold, 14)),send(H,  font, font(times, bold, 14)),send(I,  font, font(times, bold, 14)),
    send(D, display, G, point(255, 70)), send(D, display, H, point(365, 70)),send(D, display, I, point(380,70)),
    new(T, text('Nombre de coups :')),
    send(T,  font, font(times, bold, 18)),
    send(D, display, T, point(290, 20)),
    new(L,text(X)),
    send(L,  font, font(times, bold, 18)),
    send(D, display, L, point(360, 50)),
    send(D, display, button('Abandonner', message(@prolog,perdu,D)),point(267,220)),
    %On crée et place dans la fenêtre un bouton permettant de donner le nombre de coups minimum pour s'en sortir
    %dans la configuration actuelle en appelant le prédicat mini avec la fenêtre de jeu en paramètre
    send(D, display, button('Nombre de coups pour \nrésoudre la configuration-ci', message(@prolog,mini,D)),point(330,95)),
    send(D, display, button('Indice pour le prochain coup', message(@prolog,pcoup,D)),point(320,170)),
    send(D, display, button('Solution (console)', message(@prolog,sol)),point(365,220)),
    DX is 12 * 40 + 20,
	DY is 6 * 40 + 20,
	send(D, size, new(_, size(DX,DY))),
	send(D, open,point(300, 300)),
	affiche_solution(D, B, 6, 0, 0).

% S'il n'y a plus rien à afficher (on a affiché toutes les cases du jeu), on s'arrête.
affiche_solution(_D, [], _, _, _) :- !.

% on affiche la sortie Exit
affiche_solution(D, Solution, Largeur, L, Largeur) :-
    L==2,
    % on calcule sa position d'affichage dans la fenêtre
    X is 40 * 6 + 15,
    Y is 40 * 2 + 16,
    new(B, text('=> Exit')),
    send(B,  font, font(times, bold, 18)),
    % et on dit à la fenêtre de l'afficher
    send(D,display, B, point(X,Y)),
    % on passe à la ligne suivante
    L1 is L+1,
    affiche_solution(D, Solution, Largeur, L1, 0).

%Dans le cas où on se retrouve au bout de la ligne
affiche_solution(D, Solution, Largeur, L, Largeur) :-
    % on passe à la ligne suivante.
    L1 is L+1,
    affiche_solution(D, Solution, Largeur, L1, 0).

%On affiche la case en fontion du numéro dans le board H
affiche_solution(D, [H | T], Largeur, L, C) :-
	%On crée un carré blanc de 40 pixels sur 40
	new(B, box(40,40)),
	%On le colorie si H ≠ 0 sinon on le laisse intact (en blanc)
    %On essaye de faire une couleur différente pour tous les nombres, sinon on place les couleurs les plus éloigné possible 
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
    ( H = 15,  send(B, fill_pattern,  colour(red)); true),
    %On appelle le prédicat click pour implémenter le clic de souris sur la case
    click(L,C,B,D),
	%On calcule la position de la case dans la fenêtre
	X is 40 * C + 10,
	Y is 40 * L + 10,
	%On dit à la fenêtre de le placer au point X, Y
	send(D, display, B, point(X,Y)),
	%On passe à la case suivante
	C1 is C+1,
	affiche_solution(D, T, Largeur, L, C1).

%Lorsque la voiture rouge est sur le point de gagner et qu'on clique pour qu'elle sorte, on appelle le prédicat gagnerf qui affichera la
%fenêtre indiquant au joueur qu'il a réussi le niveau
click(2,4,B,D) :- board(Board), nth0(16,Board,1), nth0(17,Board,0), 
    send(B, recogniser,click_gesture(left,'',single,message(@prolog,gagnerf,D))), !.

%Ce prédicat va implémenter le clic sur la case B, ce qui va appeller le prédicat scoreAug, supprimer la fenêtre actuelle (remplacé par la 
%nouvelle où la voiture a bougé) et le prédicat bouger qui lui appelle le prédicat mouvement
click(L,C,B,D) :- C1 is C+1, L1 is L+1, 
    send(B, recogniser,click_gesture(left,'',single,and(message(@prolog,scoreAug),message(@prolog,bouger,C1,L1),message(D,destroy)))).

%Prédicat où on crée la fenêtre informant au joueur qu'il a réussi le niveau et où il peut choisir entre plusieur continuité du jeu 
%(Arreter, autre niveau ou même niveau à tenter)
gagnerf(D) :- 
    score(X),
    scoreMini(Y),
    new(F, window('Rush Hour Winner')),
    send(F, size, size(650, 200)),
    new(T, text('Félicitations! Vous avez réussi à faire sortir la voiture rouge et gagner le niveau de ce jeu!')),
    send(F, display, T, point(80, 20)),
    new(G, text('Vous avez réussi à vous en sortir en : ')),
    send(F, display, G, point(80, 35)),
    X1 is X+1,
    new(I, text(X1)),
    send(F, display, I, point(300, 35)),
    new(L, text('coups')),
    send(F, display, L, point(320, 35)),
    new(V, text('Le nombre de coups minimum pour réussir ce niveau est de : ')),
    send(F, display, V, point(80, 50)),
    new(Z, text(Y)),
    send(F, display, Z, point(430, 50)),
    new(J, text('coups')),
    send(F, display, J, point(450, 50)),
    %On affiche le score du joueur 
    new(U, text('Score : ')),
    send(U,  font, font(times, bold, 14)),
    send(F, display, U, point(400, 35)),
    %Le score correspond au ratio nombre de coups minimum sur le nombre de coups réalisé multiplié par 100 (arrondi à l'unité)
    S is round((Y/X1)*100),
    new(P, text(S)),
    send(P,  font, font(times, bold, 14)),
    send(F, display, P, point(450, 35)),
    new(A, text('/ 100')),
    send(A,  font, font(times, bold, 14)),
    send(F, display, A, point(475, 35)),
    %On crée et place dans la fenêtre un bouton permettant d'arrêter de jouer en supprimant les fenêtres restantes
    send(F, display, button('Arreter de jouer', and(message(F, destroy),message(D,destroy))),point(80,120)),
    partieEnCours(M),
    %On crée et place dans la fenêtre un slider permettant de choisir un nouveau niveau à réaliser
    send(F, display, new(Niveau, slider(niveau, 1, 10,M)), point(250,130)),
    %On crée et place dans la fenêtre un bouton permettant de lancer la nouvelle partie avec le niveau sélectionné dans le slider
    send(F, display, button('Tenter de faire cet autre niveau', 
        and(message(@prolog,demarrer,Niveau?selection),message(F,destroy),message(D,destroy))),point(300,160)),
    %On crée et place dans la fenêtre un bouton permettant de lancer une nouvelle partie du même niveau
    send(F, display, button('Retenter de résoudre le même niveau', 
        and(message(@prolog,demarrer,M),message(F,destroy),message(D,destroy))),point(250,80)),
    send(F, open).

%Prédicat où on crée la fenêtre informant au joueur qu'il a choisi d'abandonner le niveau et où il peut choisir entre plusieur continuité 
%du jeu (Arreter, autre niveau ou même niveau à retenter)
perdu(D) :- 
    score(X),
    scoreMini(Y),
    new(F, window('Rush Hour Loser')),
    send(F, size, size(650, 200)),
    new(T, text('Vous avez décidé d''abandonner! Vous n''avez pas réussi à faire sortir la voiture rouge ! :-/')),
    send(F, display, T, point(80, 20)),
    new(G, text('Vous avez réalisé durant ce défi : ')),
    send(F, display, G, point(80, 35)),
    new(I, text(X)),
    send(F, display, I, point(280, 35)),
    new(L, text('coups')),
    new(V, text('Le nombre de coups minimum pour réussir ce niveau est de : ')),
    send(F, display, V, point(80, 50)),
    new(Z, text(Y)),
    send(F, display, Z, point(430, 50)),
    new(J, text('coups')),
    send(F, display, J, point(450, 50)),
    send(F, display, L, point(300, 35)),
    send(F, display, button('Arreter de jouer', and(message(F, destroy),message(D,destroy))),point(80,120)),
    partieEnCours(M),
    send(F, display, new(Niveau, slider(niveau, 1, 10,M)), point(250,130)),
    send(F, display, button('Tenter de faire cet autre niveau', 
        and(message(@prolog,demarrer,Niveau?selection),message(F,destroy),message(D,destroy))),point(300,160)),
    send(F, display, button('Retenter de résoudre le même niveau', 
        and(message(@prolog,demarrer,M),message(F,destroy),message(D,destroy))),point(250,80)),
    send(F, open).