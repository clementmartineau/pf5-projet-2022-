# Membres du groupe :

Fafa BLIVI - @blivi - 22007283

Clément MARTINEAU - @martinec - 22003711

# Fonctionnalités :

Nous avons implémenté la totalité du sujet minimal. Ainsi nous avons implémentés fonctionnalités suivantes :
- Création d'un état initial en fonction du nom de la partie lu en ligne de commande. Cet état contient un dépôt, des colonnes, des registres et un historique des coups.
- Création de la permutation
- Validation d'un fichier solution
- Recherche de solutions

Attention, nous pouvons cependant préciser qu’il reste des erreurs dans la recherche de solutions. Cette partie du projet ne donne le bon résultat qu'à une partie des tests. Sur certains, une boucle infinie se met en place, et le programme ne se termine jamais. A contrario, les parties insolubles sont bien trouvées, et certaines parties solubles aussi.
De plus, nous n’avons pas mis en place les “heuristiques” et donc la sortie d’erreur “ECHEC”.

# Compilation & exécution : 
Notre projet se compile avec le makefile original. il faut d’abord faire `make`, ensuite `./run [NomDuJeu].[Seed] [-search|-check] [Fichier]`

# Découpage modulaire : 
Nous avons d'abord créé un module "Etat" qui sert à créer l'état initial du jeu et à effectuer les changements qui concernent cet état.
Ensuite, nous avons créé un module "GameAction" qui sert à gérer les actions liées au jeu notamment l'exécution des coups. Ce module contient également toutes les fonctions nécessaires à la validation d'un fichier solution (l'option -check).
Enfin, nous avons ajouté un module "XpatSearch" qui contient les fonctions utiles à la recherche de solutions (l'option -search)

# Organisation du travail : 
Pour le jalon un, Clément s’est chargé de shuffle et de rendre terminal les FArray et Fifo pendant que Fafa s’est occupée de la configuration du jeu ainsi que de l’option check et de la gestion des coups.
Pour le jalon deux, la tâche “Search” et plus précisément l’algorithme principal et les fonctions qui en découlent ont été allouées à Clément et Fafa s’est occupée de la recherche de tous les coups possibles à chaque État.
Au niveau de la chronologie, le jalon un a été commencé une semaine en avance tandis que à cause des examens et d’autres projets, le jalon deux a été commencé seulement 2 jours avant la date du rendu.
