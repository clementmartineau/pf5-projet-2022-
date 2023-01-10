open Card
open Etat

type pos =
| Reg of Etat.registres
| Col of Etat.colonnes

type arrivee =
| Carte of Card.card
| PlaceVide of string

(* type coup = {pos1 : pos ; pos2 : pos; num : int } *)
type coup = (Card.card * arrivee)

let depart s = Card.of_num (int_of_string s)

let arrivee s =
    if s = "V" || s = "T" then PlaceVide(s) 
    else Carte(Card.of_num (int_of_string s))

let creer_un_coup s =
    let tab = String.split_on_char ' ' s in
    match tab with
    | x :: y :: t -> (depart x, arrivee y)
    | _ -> failwith "Error"

let rec creer_coup tab =
    match tab with
    | [] -> []
    | h :: t -> creer_un_coup h :: creer_coup t 

let est_dans_une_colonne colonne card =
    if colonne = [] then false else List.hd colonne = card

let est_dans_colonnes colonnes card =
    let rec aux colonnes i =
        if i = FArray.length colonnes then (-1)
        else 
            if est_dans_une_colonne (FArray.get colonnes i) card then i
            else aux colonnes (i+1)
    in aux colonnes 0

let colonne_vide colonnes =
    let rec aux colonnes i =
        if i = FArray.length colonnes then (-1)
        else 
            if FArray.get colonnes i = [] then i
            else aux colonnes (i+1)
    in aux colonnes 0

let rec est_dans_un_registre registres card i =
    if i = FArray.length registres then -1
    else match FArray.get registres i with
         | None -> est_dans_un_registre registres card (i+1)
         | Some x -> if x = card then i else est_dans_un_registre registres card (i+1)

let est_dans_registres registres card =
    match registres with
    | None -> -1
    | Some x -> est_dans_un_registre x card 0

let rec place_vide_un_registre registres i =
    if i = FArray.length registres then (-1)
    else match FArray.get registres i with
         | None -> i
         | Some x -> place_vide_un_registre registres (i+1)

let registre_vide registres =
    match registres with
    | None -> -1
    | Some x -> place_vide_un_registre x 0

let regles_colonne game coup  =
    let depart = fst coup and arrivee = snd coup in
    match game with
    | "Freecell" -> fst depart = ((fst arrivee) - 1)  &&
                  ((snd depart = Trefle || snd depart = Pique) && (snd arrivee = Coeur || snd arrivee = Carreau) ||
                  (snd arrivee = Trefle || snd arrivee = Pique) && (snd depart = Coeur || snd depart = Carreau))
    | "Baker" -> fst depart = ((fst arrivee) - 1)
    | _ -> fst depart = ((fst arrivee) - 1)  && snd depart = snd arrivee

let regles_colonne_vide colonnes game coup =
    let depart = fst coup and arrivee = snd coup in
    match game with
    | "Freecell" -> colonne_vide colonnes <> -1
    | "Seahaven" -> colonne_vide colonnes <> -1 && fst depart = 13
    | _ -> false

let coup_valide etat coup game =
    let depart = fst coup and arrivee = snd coup in
    if est_dans_registres etat.registres depart <> (-1) || est_dans_colonnes etat.colonnes depart <> (-1)
    then match arrivee with
         | PlaceVide(x) -> 
            (x = "V" && regles_colonne_vide etat.colonnes game coup) || (x = "T" && registre_vide etat.registres <> (-1))
         | Carte(x) -> est_dans_colonnes etat.colonnes x <> (-1) && regles_colonne game (depart, x)
    else false

let normalisation_colonne colonne depot =
    if colonne = [] then (colonne, depot)
    else let c = List.hd colonne in
    match snd c with
    | Trefle -> if depot.trefle = (fst c - 1) then let d = {trefle = depot.trefle + 1; pique = depot.pique; coeur = depot.coeur; carreau = depot.carreau}
            in (List.tl colonne, d) else (colonne, depot)
    | Pique -> if depot.pique = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique + 1; coeur = depot.coeur; carreau = depot.carreau}
            in (List.tl colonne, d) else (colonne, depot)
    | Coeur -> if depot.coeur = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique; coeur = depot.coeur + 1; carreau = depot.carreau}
            in (List.tl colonne, d) else (colonne, depot)
    | Carreau -> if depot.carreau = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique; coeur = depot.coeur; carreau = depot.carreau + 1}
            in (List.tl colonne, d) else (colonne, depot)

let normalisation_colonnes colonnes depot =
    let rec aux colonnes depot i =
        if i = FArray.length colonnes then (colonnes, depot)
        else let x = normalisation_colonne (FArray.get colonnes i) depot
        in aux (FArray.set colonnes i (fst x)) (snd x) (i + 1)
    in aux colonnes depot 0

let normalisation_registre carte depot =
    if carte = None then (None, depot)
    else let c = Option.get carte in
    match snd c with
    | Trefle -> if depot.trefle = (fst c - 1) then let d = {trefle = depot.trefle + 1; pique = depot.pique; coeur = depot.coeur; carreau = depot.carreau} in
                (None, d) else (carte, depot)
    | Pique -> if depot.pique = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique + 1; coeur = depot.coeur; carreau = depot.carreau} in
                (None, d) else (carte, depot)
    | Coeur -> if depot.coeur = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique; coeur = depot.coeur + 1; carreau = depot.carreau} in
                (None, d) else (carte, depot)
    | Carreau -> if depot.carreau = (fst c - 1) then let d = {trefle = depot.trefle; pique = depot.pique; coeur = depot.coeur; carreau = depot.carreau + 1} in
                (None, d) else (carte, depot)

let normalisation_registres registres depot =
    match registres with
    | None -> (registres, depot)
    | Some x ->
        let rec aux registres depot i = 
            if i = FArray.length registres then (Some registres, depot)
            else let reg = (normalisation_registre (FArray.get registres i) depot) in
            aux (FArray.set registres i (fst reg)) (snd reg) (i+1)
        in aux x depot 0

let une_normalisation etat =
    let new_col = normalisation_colonnes etat.colonnes etat.depot in
    let new_reg = normalisation_registres etat.registres (snd new_col)
    in {depot = snd new_reg; colonnes = fst new_col; registres = fst new_reg}

let rec normalisation etat =
    let new_etat = une_normalisation etat in 
    if new_etat = etat then etat else normalisation new_etat

let enleve_colonne colonnes carte i = 
    let x = FArray.get colonnes i in FArray.set colonnes i (List.tl x)

let enleve_registre registres carte i =
    let r = Option.get registres in Some (FArray.set r i None)

let ajoute_registre registres carte =
    Some (FArray.set (Option.get registres) (registre_vide registres) (Some carte))

let ajoute_colonne colonnes depart arrivee =
    let i = est_dans_colonnes colonnes arrivee in
    let x = FArray.get colonnes i in
    FArray.set colonnes i (depart :: x)

let ajoute_colonne_vide colonnes carte =
    let i = colonne_vide colonnes in
    let x = FArray.get colonnes i in
    FArray.set colonnes i (carte :: x)

let jouer_coup etat coup =
    let depart = fst coup and arrivee = snd coup in
    let i0 = est_dans_colonnes etat.colonnes depart and i1 = est_dans_registres etat.registres depart in
    
    let new_col = if i0 <> -1 then enleve_colonne etat.colonnes depart i0 else etat.colonnes
    and new_reg = if i1 <> -1 then enleve_registre etat.registres depart i1 else etat.registres in
    
    let new_etat = etat_make etat.depot new_col new_reg in
    
    match arrivee with
    | Carte(x) -> let new_col_2 = ajoute_colonne new_etat.colonnes depart x in
                    etat_make etat.depot new_col_2 new_etat.registres
    | PlaceVide(x) -> 
        match x with
        | "V" -> let new_col_2 = ajoute_colonne_vide new_etat.colonnes depart in
                    etat_make etat.depot new_col_2 new_etat.registres
        | "T" -> let new_reg_2 = ajoute_registre new_etat.registres depart in
                    etat_make etat.depot new_etat.colonnes new_reg_2
        | _ -> failwith "Erreur de syntaxe des coups"

let jeu etat coup =
    let e = jouer_coup etat coup in normalisation e

let configuration_gagnante etat =
    let d = etat.depot in d.trefle = 13 && d.pique = 13 && d.coeur = 13 && d.carreau = 13

let rec check etat game coups i =
    match coups with
    | [] -> if configuration_gagnante etat then (etat, 0) else (etat, i)
    | h :: t -> if coup_valide etat h game then check (jeu etat h) game t (i+1)
                else (etat, i)