type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (Card.card list) FArray.t
type registres = ((Card.card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres}

module Etats = Set.Make (struct type t = etat let compare = compare_etat end)


val etat_make : depot -> colonnes -> registres -> etat
val etat_init : string -> int list -> etat
val etat_to_string : etat -> string
val compare_etat : etat -> etat -> int