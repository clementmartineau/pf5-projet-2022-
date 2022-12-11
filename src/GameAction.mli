type pos =
| Reg of Etat.registres
| Col of Etat.colonnes

type arrivee =
| Carte of Card.card
| PlaceVide of string

(* type coup = {pos1 : pos ; pos2 : pos; num : int } *)
type coup = (Card.card * arrivee)

val creer_coup : string list -> coup list
val coup_valide : Etat.etat -> coup -> string -> bool
val normalisation : Etat.etat -> Etat.etat
val jouer_coup : Etat.etat -> coup -> Etat.etat
val jeu : Etat.etat -> coup -> Etat.etat
val check : Etat.etat -> string -> coup list -> int -> (Etat.etat * int)