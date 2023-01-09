
(* fonction qui renvoie la solution sous forme de couple d'un optionnel string list et d'un string représentant la sortie obtenue *)
let get_solution etat =




(* une fonction pour calculer le nombre d'états possible *)





(*
II.2 Méthode de recherche


Lors d'une partie, les états atteignables peuvent être vus comme les noeuds d'un graphe,
et les coups légaux lors de la partie sont alors les arêtes de ce graphe orienté.

On cherche donc à relier l'état initial de la partie avec l'état final gagnant (celui où toutes les cartes sont au dépôt),
et trouver un chemin reliant ces deux états (s'il en existe un). Il s'agit a priori d'un problème classique de l'algorithmique des graphes.
Mais même si ce graphe des états d'une partie est forcément fini, il sera presque toujours gigantesque,
ce qui oblige à le visiter prudemment, "au vol", en optimisant les données manipulées et les méthodes utilisées.
En particulier les états atteignables ne seront créés que progressivement.

L'idée générale est celle d'un parcours de graphe. A tout moment, on a un ensemble d'états restant à visiter (au début juste l'état initial),
et un ensemble d'états déjà traités (au début vide).
La visite d'un état consiste à calculer les états atteignables par des coups légaux à partir de cet état,
puis mettre ces nouveaux états parmi ceux à visiter (sauf ceux qui ne sont pas si nouveaux que cela mais au contraire sont déjà parmi les traités),
puis mettre l'état qui vient d'être visité parmi les traités.

On rappelle que les états qu'on manipule devront toujours être normalisés (i.e. avoir le maximum possible de cartes mis au dépôt).
En particulier, l'état initial de la partie devra être normalisé avant le début de la recherche.
Ensuite, chaque action d'un coup légal sur un état en cours de visite devra être suivi d'une possible renormalisation.
Cela permet de limiter le nombre d'états à considérer et de coups à explorer. Pour les règles Seahaven et Midnight Oil,
c'est compatible avec une recherche exhaustive, mais pas pour FreeCell et Baker's Dozen
(mais en pratique cela ne semble pas empêcher la découverte de solution).

On appellera score d'un état le nombre de cartes qu'il a dans son dépôt. Il y a un seul état de score 52, c'est la configuration gagnante.
Si la recherche rencontre cet état, une solution existe, et on arrête la recherche.
Comme indiqué en partie I, il est recommandé d'avoir dans la représentation d'un état une zone donnant un historique des coups ayant mené à cet état.
Dans ce cas, l'historique des coups de l'état gagnant donne alors une solution à la partie explorée.
Attention, cette approche nécessite par contre d'utiliser une comparaison particulière entre états, et non Stdlib.compare, voir détails plus bas.

Maintenant, si on ne rencontre jamais l'état gagnant, et que l'ensemble des états restant à visiter devient vide,
c'est que la recherche est terminée sans solution. Et cette recherche a été a priori exhaustive,
sauf si l'on a utilisé des heuristiques supprimant brutalement des états peu prometteurs (voir la section "Conseils" ci-dessous).

Dans quel ordre visiter les états ? Pour une visite exhaustive et infructueuse, peu importe, au final on sera passé partout.
Par contre si une solution existe, autant essayer d'y aboutir au plus vite, et éviter la visite d'états qui ne donneront rien,
ou alors bien plus tard. A vous d'implémenter une approche aussi efficace que possible sur de véritables parties (voir par exemple les tests fournis).

Une méthode simple à mettre en oeuvre (mais pas forcément rapide) consiste à procéder par profondeur progressive,
ce qui revient à faire un parcours en largeur d'abord. La profondeur est ici le nombre de coup minimal pour atteindre un état.
La profondeur 0, c'est l'état initial, et la profondeur (n+1), ce sont les états atteignables depuis l'un des états de profondeur n,
et pas déjà rencontrés auparavant. On peut calculer progressivement ces ensembles d'états à des profondeurs croissantes,
ainsi que l'ensemble des anciens états déjà rencontrés à profondeur plus faible.

En procédant ainsi, on est sûr en outre d'obtenir une solution qui aura un nombre de coup minimal (même si ce n'est pas imposé).
Une autre méthode possible est de s'intéresser aux scores des états, et de visiter d'abord les états ayant les scores les plus élevés.
Les scores étant entre 0 et 52, on pourra ranger les états encore à visiter selon leurs scores.
Attention, un tel parcours peut souvent passer par des "impasses" (états aux scores élevés mais sans coups légaux restants, ou seulement vers des états déjà vus).
Il faut alors retourner à des états restants à visiter mais ayant des scores un peu moins bon.
Pour indiquer à l'utilisateur où en est la recherche, quelques affichages intermédiaires pourront être utiles, ni trop ni trop peu.
*)