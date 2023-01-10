open Card
open Etat

type arrivee =
| Carte of card
| PlaceVide of string
| Vide of int

type coup = card * arrivee

(* "depart" permet de convertir une string en carte *)
let depart mot = of_num (int_of_string mot)

(* "arrivee" permet de convertir une string en "arrivee" donc soit une carte soit un string *)
let arrivee mot =
    if mot = "V" || mot = "T" then PlaceVide(mot) 
    else Carte(of_num (int_of_string mot))

(* "creer_un_coup" permet de convertir une ligne du fichier solution en un coup *)
let creer_un_coup ligne =
    let tab = String.split_on_char ' ' ligne in
    match tab with
    | mot1 :: mot2 :: t -> (depart mot1, arrivee mot2)
    | _ -> failwith "Erreur de syntaxe des coups"

(* "creer_coups" permet de convertir toutes les lignes du fichier solution en une liste de coups *)
let rec creer_coups lignes =
    match lignes with
    | [] -> []
    | h :: t -> creer_un_coup h :: creer_coups t 

(* "est_dans_une_colonne" renvoie true si la carte demandée est au sommet de la colonne *)
let est_dans_une_colonne colonne card =
    if colonne = [] then false else List.hd colonne = card

(* "est_dans_colonnes" renvoie true si la carte est au sommet d'au moins une colonne *)
let est_dans_colonnes colonnes card =
    let rec aux colonnes i =
        if i = FArray.length colonnes then (-1)
        else 
            if est_dans_une_colonne (FArray.get colonnes i) card then i
            else aux colonnes (i+1)
    in aux colonnes 0

(* "colonne_vide" renvoie l'indice de la première colonne vide et -1 s'il n'y a en a aucune *)
let colonne_vide colonnes =
    let rec aux colonnes i =
        if i = FArray.length colonnes then -1
        else 
            if FArray.get colonnes i = [] then i
            else aux colonnes (i+1)
    in aux colonnes 0

(* "est_dans_un_registre" renvoie l'indice du premier registre qui contient la carte et -1 s'il n'y en a aucune *)
let rec est_dans_un_registre registres card n i =
    if i = n then -1
    else match FArray.get registres i with
         | None -> est_dans_un_registre registres card n (i+1)
         | Some x -> if x = card then i else est_dans_un_registre registres card n (i+1)

(* "est_dans_registres" a la même fonction que "est_dans_un_registre" sauf qu'elle vérifie s'il y a un registre *)
let est_dans_registres registres card =
    match registres with
    | None -> -1
    | Some x -> est_dans_un_registre x card (FArray.length x) 0

(* "place_vide_registre" renvoie l'indice du premier registre vide *)
let rec place_vide_registre registres n i =
    if i = n then (-1)
    else match FArray.get registres i with
         | None -> i
         | Some x -> place_vide_registre registres n (i+1)

(* "registre_vide" a la même fonction que "place_vide_registre" sauf qu'elle vérifie s'il y a un registre *)
let registre_vide registres =
    match registres with
    | None -> -1
    | Some x -> place_vide_registre x (FArray.length x) 0

(* "regles_colonne" renvoie true si le coup respecte les règles concernant les colonnes non vides en fonction du mode de jeu *)
let regles_colonne game coup =
    let d = fst coup and a = snd coup in
    match game with
    | "Freecell" -> fst d = ((fst a) - 1) &&
                  ((snd d = Trefle || snd d = Pique) && (snd a = Coeur || snd a = Carreau) ||
                   (snd a = Trefle || snd a = Pique) && (snd d = Coeur || snd d = Carreau))
    | "Baker" -> fst d = ((fst a) - 1)
    | _ -> fst d = ((fst a) - 1)  && snd d = snd a

(* "regles_colonne_vide" renvoie true si le coup respecte les règles concernant les colonnes vides en fonction du mode de jeu *)
let regles_colonne_vide colonnes game coup =
    let depart = fst coup and arrivee = snd coup in
    match game with
    | "Freecell" -> colonne_vide colonnes <> -1
    | "Seahaven" -> colonne_vide colonnes <> -1 && fst depart = 13
    | _ -> false

(* "normalisation_une_colonne" procède à la mise au dépôt d'une colonne et renvoie la nouvelle colonne et le nouveau dépôt *)
let normalisation_une_colonne colonne depot =
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

(* "normalisation_colonnes" procède à la mise au dépôt de toutes les colonnes et renvoie les nouvelles colonnes et le nouveau dépôt *)
let normalisation_colonnes colonnes depot =
    let rec aux colonnes depot i =
        if i = FArray.length colonnes then (colonnes, depot)
        else let x = normalisation_une_colonne (FArray.get colonnes i) depot
        in aux (FArray.set colonnes i (fst x)) (snd x) (i + 1)
    in aux colonnes depot 0

(* "normalisation_un_registre" procède à la mise au dépôt d'un registre et renvoie le nouveau registre et le nouveau dépôt *)
let normalisation_un_registre carte depot =
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

(* "normalisation_registres" procède à la mise au dépôt de toutes les registres et renvoie les nouvelles registres et le nouveau dépôt *)
let normalisation_registres registres depot =
    match registres with
    | None -> (registres, depot)
    | Some x ->
        let rec aux registres depot i = 
            if i = FArray.length registres then (Some registres, depot)
            else let reg = (normalisation_un_registre (FArray.get registres i) depot) in
            aux (FArray.set registres i (fst reg)) (snd reg) (i+1)
        in aux x depot 0

(* "une_normalisation" procède à la mise au dépot de tout le jeu *)
let une_normalisation etat =
    let new_col = normalisation_colonnes etat.colonnes etat.depot in
    let new_reg = normalisation_registres etat.registres (snd new_col)
    in etat_make (snd new_reg) (fst new_col) (fst new_reg) (etat.historique)

(* "normalisation" procède à la mise au dépot de tout le jeu jusqu'à que ce ne soit plus possible*)
let rec normalisation etat =
    let new_etat = une_normalisation etat in 
    if new_etat = etat then etat else normalisation new_etat

(* "enleve_colonne" enlève la carte au sommet de la colonne d'indice i *)
let enleve_colonne colonnes i = 
    let x = FArray.get colonnes i in FArray.set colonnes i (List.tl x)

(* "ajoute_colonne" ajoute la carte de depart au sommet de la colonne qui contient la carte d'arrivee *)
let ajoute_colonne colonnes depart arrivee =
    let i = est_dans_colonnes colonnes arrivee in
    FArray.set colonnes i (depart :: (FArray.get colonnes i))

(* "ajoute_colonne_vide" ajoute la carte dans une colonne vide *)
let ajoute_colonne_vide colonnes carte =
    let i = colonne_vide colonnes in
    FArray.set colonnes i (carte :: (FArray.get colonnes i))

(* "enleve_registre" enlève la carte contenu dans le registre d'indice i *)
let enleve_registre registres i =
    let r = Option.get registres in Some (FArray.set r i None)

(* "ajoute_registre" ajoute la carte dans un registre vide *)
let ajoute_registre registres carte =
    Some (FArray.set (Option.get registres) (registre_vide registres) (Some carte))

(* "coup_valide" renvoie true si le coup est possible en fonction des regles jeu et sa distribution *)
let coup_valide etat coup game =
    let depart = fst coup and arrivee = snd coup in
    let a = 
    match arrivee with
    | PlaceVide(x) -> 
            (x = "V" && regles_colonne_vide etat.colonnes game coup) || 
            (x = "T" && registre_vide etat.registres <> (-1))
    | Carte(x) -> depart <> x && regles_colonne game (depart, x) && est_dans_colonnes etat.colonnes x <> (-1)
    | _ -> false
    in a && (est_dans_colonnes etat.colonnes depart <> (-1) || est_dans_registres etat.registres depart <> (-1))

(* "coup_to_string" permet de transformer un coup en string sous le format des fichiers de solution *)
let coup_to_string coup =
    let (d,c) = coup in
    match c with
    | Carte(x) -> string_of_int (Card.to_num d) ^ " " ^ string_of_int (Card.to_num x) ^ "\n"
    | PlaceVide(x) -> string_of_int (Card.to_num d) ^ " " ^ x ^ "\n"
    | _ -> failwith "Coup invalide"

(* "jouer_coup" renvoie l'etat apres l'execution d'un coup *)
let jouer_coup etat coup =
    let depart = fst coup and arrivee = snd coup in
    let i0 = est_dans_colonnes etat.colonnes depart 
    and i1 = est_dans_registres etat.registres depart in
    
    let new_col = if i0 <> -1 then enleve_colonne etat.colonnes i0 else etat.colonnes
    and new_reg = if i1 <> -1 then enleve_registre etat.registres i1 else etat.registres in
    
    let new_etat = etat_make etat.depot new_col new_reg (etat.historique @ [coup_to_string coup]) in
    
    match arrivee with
    | Vide(_) -> failwith "Coup invalide"
    | Carte(x) -> let new_col_2 = ajoute_colonne new_etat.colonnes depart x in
                    etat_make etat.depot new_col_2 new_etat.registres new_etat.historique
    | PlaceVide(x) -> 
        match x with
        | "V" -> let new_col_2 = ajoute_colonne_vide new_etat.colonnes depart in
                    etat_make etat.depot new_col_2 new_etat.registres new_etat.historique
        | "T" -> let new_reg_2 = ajoute_registre new_etat.registres depart in
                    etat_make etat.depot new_etat.colonnes new_reg_2 new_etat.historique
        | _ -> failwith "Erreur de syntaxe des coups"

(* "configuration_gagnante" renvoie true si la configuration de l'etat est gagnante *)
let configuration_gagnante etat =
    let d = etat.depot 
    in d.trefle = 13 && d.pique = 13 && d.coeur = 13 && d.carreau = 13

(* "check" renvoie un couple contenant un etat et un int : l'etat correspond à l'état atteint après l'exécution des coups valide
et i correspond au nombre de coups non valides et vaut 0 si l'etat final est gagnant *)
let rec check etat game coups i =
    let e = normalisation etat in
    match coups with
    | [] -> if configuration_gagnante e then (e, 0) else (e, i)
    | coup :: t -> if coup_valide e coup game 
                   then check (jouer_coup e coup) game t (i+1)
                   else (e, i)



