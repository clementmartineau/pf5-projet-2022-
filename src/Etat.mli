type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (Card.card Fifo.t) FArray.t
type registres = ((Card.card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres}

val depot_init : depot
val depot_to_string : depot -> string

val une_colonne_init : int -> colonnes
val remplir_une_colonne : Card.card Fifo.t -> int -> int list -> (Card.card Fifo.t * int list)
val une_colonne_to_string : Card.card Fifo.t -> string
val remplir_colonnes : colonnes -> int list -> (int -> int) -> (colonnes * int list)
val colonnes_init : string -> int list -> (colonnes * int list)
val colonnes_to_string : colonnes -> string

val remplir_registres : (Card.card option) FArray.t -> int -> int list -> (Card.card option) FArray.t
val registres_init : string -> int list -> registres
val registres_to_string : registres -> string

val etat_init : string -> int list -> etat
val etat_to_string : etat -> string