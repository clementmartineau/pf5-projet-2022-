open Etat
open GameAction
module Etats = Set.Make (struct type t = Etat.etat let compare = Etat.compare_etat end)


let coup_col_to_reg n i a_visiter etat game deja_traites = (* ajouter la carte du haut de la col n au registre i si possible *)
    let coup = let col = FArray.get etat.colonnes n in
        if col = [] then (Card.of_num (-1), Vide(0))
        else (List.hd col, PlaceVide("T"))
    in
    if GameAction.coup_valide etat coup game then (* si coup valide *)
        let new_etat = GameAction.jouer_coup etat coup |> GameAction.normalisation in (* on créer l'état *)
        if Etats.mem new_etat deja_traites then a_visiter(* on vérifie si l'état est deja traité *)
        else Etats.add new_etat a_visiter (* on l'ajoute et on renvoie*)
    else a_visiter

let coup_col_to_col n i a_visiter etat game deja_traites = (* ajouter la carte du haut de la col n a la col i si possible *)
    if i = n then a_visiter (* si col n = col i -> return *)
    else
        let coup =
            let col1 = FArray.get etat.colonnes n and col2 = FArray.get etat.colonnes i in
            if col1 = [] then (Card.of_num (-1), Vide(0))
            else 
                if col2 = [] then (List.hd col1, PlaceVide("V")) 
                else (List.hd col1, Carte(List.hd col2))
        in
        if GameAction.coup_valide etat coup game then (* si coup valide *)
            let new_etat = GameAction.jouer_coup etat coup |> GameAction.normalisation in (* on créer l'état *)
            if Etats.mem new_etat deja_traites then a_visiter(* on vérifie si l'état est deja traité *)
            else Etats.add new_etat a_visiter (* on l'ajoute et on renvoie*)
        else a_visiter

let coup_reg_to_col n i a_visiter etat game deja_traites = (* ajouter la carte du reg n a la col i si possible *)
        let coup = 
            let reg = etat.registres in
            if reg = None then (Card.of_num (-1), Vide(0))
            else
                let carte = FArray.get (Option.get reg) n in
                if carte = None then (Card.of_num (-1), Vide(0))
                else 
                    let col = FArray.get etat.colonnes i in
                    if col = [] then ( Option.get carte , PlaceVide("V"))
                    else ( Option.get carte , Carte(List.hd col))
        in
        if GameAction.coup_valide etat coup game then (* si coup valide *)
            let new_etat = GameAction.jouer_coup etat coup |>  GameAction.normalisation in (* on créer l'état *)
            if Etats.mem new_etat deja_traites then a_visiter(* on vérifie si l'état est deja traité *)
            else Etats.add new_etat a_visiter (* on l'ajoute et on renvoie*)
        else a_visiter


let ajouter_coups_depuis_col_aux n a_visiter etat game deja_traites = (* on ajoute tous les coups depuis une colonne*)
    let rec aux_col_reg n i a_visiter etat game deja_traites= (* ajout carte col n dans reg i*)
        if etat.registres  = None || i = FArray.length (Option.get etat.registres) then a_visiter (* apres avoir visité tous les reg, on return*)
        else
            let a_visiter = coup_col_to_reg n i a_visiter etat game deja_traites (* on ajoute le coup col n au reg i *)
            in aux_col_reg n (i + 1) a_visiter etat game deja_traites (* on fait l'appel suivant avec col n et reg i + 1*)
    in let rec aux_col_col n i a_visiter etat game deja_traites = (* ajout carte col n dans col i*)
        if i = FArray.length etat.colonnes then a_visiter (* apres avoir visité toutes les col on return *)
        else
            let a_visiter = coup_col_to_col n i a_visiter etat game deja_traites (* on ajoute le coup col n to col i*)
            in aux_col_col n (i + 1) a_visiter etat game deja_traites (* on fait l'appel suivant avec col n et col i + 1 *)
    in let a_visiter = aux_col_col n 0 a_visiter etat game deja_traites
    in aux_col_reg n 0 a_visiter etat game deja_traites



let ajouter_coups_depuis_reg_aux n a_visiter etat game deja_traites = (* on ajoute tous les coups depuis un registre*)
    let rec aux_reg_col n i a_visiter etat game deja_traites = (* ajout carte col n dans col i*)
        if i = FArray.length etat.colonnes then a_visiter (* apres avoir visité toutes les col on return *)
        else
            let a_visiter = coup_reg_to_col n i a_visiter etat game deja_traites (* on ajoute le coup reg n to col i*)
            in aux_reg_col n (i + 1) a_visiter etat game deja_traites (* on fait l'appel suivant avec reg n et col i + 1 *)

    in aux_reg_col n 0 a_visiter etat game deja_traites
(*
Fonction qui depuis un etat, ajoute tous les états résultants de tous les coups possibles a un Set a_visiter.
Les états ajoutés doivent être normalisés

return a_visiter

pour ajouter un etat faire Etats.add a_visiter etat_du_coup
(je sais pas si ça marche parce que c'est pas le bon document, peut-etre open XpatSearch?)
*)
let ajouter_tous_coups a_visiter etat game deja_traites =
    let rec ajouter_coups_depuis_col n a_visiter etat game deja_traites = (* Parcours des 8 colonnes *)
        if n = FArray.length etat.colonnes then a_visiter
        else
            let a_visiter = ajouter_coups_depuis_col_aux n a_visiter etat game deja_traites
            in ajouter_coups_depuis_col (n + 1) a_visiter etat game deja_traites
    in let a_visiter = ajouter_coups_depuis_col 0 a_visiter etat game deja_traites in
    let rec ajouter_coups_depuis_reg n a_visiter etat game deja_traites =
        if etat.registres =  None || n = FArray.length (Option.get etat.registres) then a_visiter
        else
            let a_visiter = ajouter_coups_depuis_reg_aux n a_visiter etat game deja_traites
            in ajouter_coups_depuis_reg (n + 1) a_visiter etat game deja_traites
    in let a_visiter = ajouter_coups_depuis_col 0 a_visiter etat game deja_traites
    in ajouter_coups_depuis_reg 0 a_visiter etat game deja_traites


let getStringSortie i =
    if i = 0 then "SUCCES"
    else if i = 1 then "ECHEC"
    else "INSOLUBLE"

(* renvoie l'état avec le meilleur score dans le Set *)
let get_meilleure_branche a_visiter =
    Etats.fold (fun x y -> if (Etat.get_score x) > (Etat.get_score y) then x else y) a_visiter (Etats.choose a_visiter)

(* Dans cette fonction on fait le parcours récursif dans l'arbre
    Il s'agit d'un parcours en profondeur car nous nous enfonçons dans une branche a chaque fois
    mais la branche choisie est celle qui a le meilleur score parmi tous les états dans a_verifier
    (voir get_meilleure_branche et get_score)
    Il s'agit de l'algorithme du plus court chemin de Djikstra
    (ou on veut l'état au plus gros score au lieu du voisin le plus proche à chaque itération)
*)
let rec parcours a_visiter deja_traites game =
    let etat = get_meilleure_branche a_visiter in (* on récupère le meilleur état*)
    if get_score etat = 52 then (* si le score est de 52, alors SUCCES*)
            (print_string (Etat.etat_to_string etat);
            (a_visiter, deja_traites, 0, etat))
    else
        let a_visiter = Etats.remove etat a_visiter in (* on le supprime de a_visiter*)
        let deja_traites = Etats.add etat deja_traites in (* on l'ajoute dans deja_traites*)
        let a_visiter = ajouter_tous_coups a_visiter etat game deja_traites in (* on ajoute les états issus de tous les coups possibles depuis l'état courant*)
                                                             (* ATTENTION : ajouter_tous_coups DOIT normaliser les états ajoutés dans à visiter*)
        if Etats.is_empty a_visiter then (a_visiter, deja_traites, 2, etat) (* si il n'y a rien a visiter, alors INSOLUBLE*)
        else parcours a_visiter deja_traites game(* on fait un appel récursif*)


(* fonction qui renvoie la solution sous forme de couple d'un string list et d'un string représentant la sortie obtenue *)
let get_solution etat game =
    let etat = GameAction.normalisation etat in (* on normalise l'état initial*)
    let a_visiter = Etats.singleton etat in (* on initialise les états a_visiter*)
    let deja_traites = Etats.empty in (* on initialise les états deja_traites*)
    let (_,_,i,etat_sortie) = (parcours a_visiter deja_traites game) (* on effectue le parcours et on renvoie le résultat voulu*)
    in (etat_sortie.historique, (getStringSortie i)) (* on renvoie un couple constitué de l'historique des coups et du string de sortie *)
