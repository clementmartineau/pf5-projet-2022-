type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (Card.card list) FArray.t
type registres = ((Card.card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres}

val etat_init : string -> int list -> etat
val etat_to_string : etat -> string