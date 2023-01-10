open Etat
module Etats = Set.Make (struct type t = Etat.etat let compare = Etat.compare_etat end)



(*
Fonction qui depuis un etat, ajoute tous les états résultants de tous les coups possibles a un Set a_visiter.
Les états ajoutés doivent être normalisés

return a_visiter

pour ajouter un etat faire Etats.add a_visiter etat_du_coup
(je sais pas si ça marche parce que c'est pas le bon document, peut-etre open XpatSearch?)
*)
let ajouter_tous_coups a_visiter etat =
    a_visiter

let getStringSortie i =
    if i = 0 then "SUCCES"
    else if i = 1 then "ECHEC"
    else "INSOLUBLE"

(* renvoie le score d'un état *)
let get_score etat =
    etat.depot.trefle + etat.depot.coeur + etat.depot.pique + etat.depot.carreau

(* renvoie l'état avec le meilleur score dans le Set *)
let get_meilleure_branche a_visiter =
    Etats.fold (fun x y -> if (get_score x) > (get_score y) then x else y) a_visiter (Etats.choose a_visiter)

(* Dans cette fonction on fait le parcours récursif dans l'arbre
    Il s'agit d'un parcours en profondeur car nous nous enfonçons dans une branche a chaque fois
    mais la branche choisie est celle qui a le meilleur score parmi tous les états dans a_verifier
    (voir get_meilleure_branche et get_score)
    Il s'agit de l'algorithme du plus court chemin de Djikstra
    (ou on veut l'état au plus gros score au lieu du voisin le plus proche à chaque itération)
*)
let rec parcours a_visiter deja_traites =
    let e = get_meilleure_branche a_visiter in
    let etat = Etats.singleton e in (* on récupère le meilleur état*)
    if get_score e = 52 then (* si le score est de 52, alors SUCCES*)
            (a_visiter, deja_traites, 0, etat)
    else
        let a_visiter_bis = Etats.remove e a_visiter in (* on le supprime de a_visiter*)
        let deja_traites_bis = Etats.add e deja_traites in (* on l'ajoute dans deja_traites*)
        let a_visiter_ter = ajouter_tous_coups a_visiter_bis etat in (* on ajoute les états issus de tous les coups possibles depuis l'état courant*)
                                                             (* ATTENTION : ajouter_tous_coups DOIT normaliser les états ajoutés dans à visiter*)
        if Etats.is_empty a_visiter then (a_visiter_ter, deja_traites_bis, 2, etat) (* si il n'y a rien a visiter, alors INSOLUBLE*)
        else parcours a_visiter_ter deja_traites_bis (* on fait un appel récursif*)



(* fonction qui renvoie la solution sous forme de couple d'un string list et d'un string représentant la sortie obtenue *)
let get_solution etat =
    let etat = GameAction.normalisation etat in (* on normalise l'état initial*)
    let a_visiter = Etats.singleton etat in (* on initialise les états a_visiter*)
    let deja_traites = Etats.empty in (* on initialise les états deja_traites*)
    match (parcours a_visiter deja_traites)  with (* on effectue le parcours et on renvoie le résultat voulu*)
    | (_,_,i,etat_sortie) -> ((Etats.choose etat_sortie).historique, (getStringSortie i)) (* on renvoie un couple constitué de l'historique des coups et du string de sortie *)