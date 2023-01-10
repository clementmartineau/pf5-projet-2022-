type arrivee =
| Carte of Card.card
| PlaceVide of string
| Vide of int

type coup = (Card.card * arrivee)

val coup_valide : Etat.etat -> coup -> string -> bool
val creer_coups : string list -> coup list
val jouer_coup : Etat.etat -> coup -> Etat.etat
val normalisation : Etat.etat -> Etat.etat
val configuration_gagnante : Etat.etat -> bool
val check : Etat.etat -> string -> coup list -> int -> Etat.etat * int