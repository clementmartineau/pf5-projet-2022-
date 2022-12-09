open Card
open Fifo
open List

type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (Card.card list) FArray.t
type registres = ((Card.card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres}

let depot_init = {trefle = 0; pique = 0; coeur = 0; carreau = 0}

let depot_to_string depot =
    "Le dépôt contient " ^ 
    string_of_int depot.trefle ^ " carte(s) trèfle, " ^
    string_of_int depot.pique ^ " carte(s) pique, " ^ 
    string_of_int depot.coeur ^ " carte(s) coeur et " ^
    string_of_int depot.carreau ^ " carte(s) carreau.\n\n"

let rec remplir_une_colonne colonne n p =
    if (n = 0) then (List.rev colonne, p)
    else remplir_une_colonne ((of_num (hd p)) :: colonne) (n-1) (tl p)

let rec une_colonne_to_string colonne =
    match colonne with
    | [] -> ""
    | h :: t -> (to_string h) ^ " " ^ une_colonne_to_string t 

let remplir_colonnes colonnes p f n =
    let rec aux colonnes i n p =
        if(i = n)
            then (colonnes, p)
        else
            let c = remplir_une_colonne (FArray.get colonnes i) (f i) p in
            let c2 = FArray.set colonnes i (fst c) in
            aux c2 (i+1) n (snd c)
    in aux colonnes 0 n p

let descendre_roi colonne =
    let rec aux roi autres c = 
        match c with
        | [] -> List.rev roi @ List.rev autres
        | h :: t -> if fst h = 13 then aux (h :: roi) autres t
                    else aux roi (h :: autres) t
    in aux [] [] colonne
    
let descendre_tous_les_rois colonnes = 
    FArray.map descendre_roi colonnes


let colonnes_init game p = 
    if game = "Freecell" 
        then remplir_colonnes (FArray.make 8 []) p (fun i -> if i mod 2 = 0 then 7 else 6) 8
    else if game = "Seahaven" 
        then remplir_colonnes (FArray.make 10 []) p (fun i -> 5) 10
    else if game = "Midnight"
        then remplir_colonnes (FArray.make 18 []) p (fun i -> if i < 17 then 3 else 1) 18
    else if game = "Baker" 
        then let c = remplir_colonnes (FArray.make 13 []) p (fun i -> 4) 13
        in ((descendre_tous_les_rois (fst c)), (snd c))
    else failwith "Invalid Game"

let colonnes_to_string colonnes =
    let rec aux colonnes n =
        if(n = FArray.length colonnes) then "\n"
        else "Colonne n°" ^ string_of_int (n+1) ^ " : " ^ une_colonne_to_string (FArray.get colonnes n) ^ "\n" ^ aux colonnes (n+1) 
    in aux colonnes 0

let remplir_registres registres n p =
    let rec aux registres n p i =
        if n = i then registres
        else aux (FArray.set (registres) i (Some(of_num(hd p)))) n (tl p) (i+1)
    in aux registres n p 0

let registres_init game p =
    if game = "Freecell" 
        then Some (FArray.make 4 None)
    else if game = "Seahaven"
        then Some (remplir_registres (FArray.make 4 None) 2 p)
    else if game = "Midnight" || game = "Baker" 
        then None
    else failwith "Invalid Game"

let registres_to_string registres = 
    match registres with
    | None -> "Ce mode de jeu ne contient pas de registre\n"
    | Some x -> 
        let rec aux x n =
        if n = FArray.length x then ""
        else
            match FArray.get x n with
            | None -> "Registre n°" ^ string_of_int (n+1) ^ " : vide\n" ^ aux x (n+1)
            | Some c -> "Registre n°" ^ string_of_int (n+1) ^ " : " ^ Card.to_string c ^ "\n" ^aux x (n+1)
        in aux x 0

let etat_init game p =
let c = colonnes_init game p in {depot = depot_init ; colonnes = fst c; registres = registres_init game (snd c)}

let etat_to_string etat = 
    depot_to_string etat.depot ^ colonnes_to_string etat.colonnes ^ registres_to_string etat.registres
