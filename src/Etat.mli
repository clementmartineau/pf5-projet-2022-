type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (Card.card list) FArray.t
type registres = ((Card.card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres; historique : string list}

val etat_make : depot -> colonnes -> registres -> string list -> etat
val etat_init : string -> int list -> etat
val etat_to_string : etat -> string
val get_score : etat -> int
val compare_etat : etat -> etat -> int