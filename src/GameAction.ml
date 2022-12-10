type pos =
| reg of Etat.registres
| col of Etat.colonnes

type coup = {pos1 : pos ; pos2 : pos; num : int }



(* essais infructueux de fonctions :(


let rec comp_depot_col dep cols index =
    match Farray.get(index) with
    | h :: t -> if h = Card.of_num (dep + 1)
                then let depot = {trefle = (dep + 1) ;
                pique = etat.depot.pique ;
                coeur = etat.depot.coeur ;
                carreau = etat.depot.carreau}
                in let colonnes = FArray.set cols index t
                in



let mise_au_depot etat =
    let dep = etat.depot.trefle in
    let rec parcours_array n =
        let l = parcours_array
         match l with
            | h :: t -> if h = Card.of_num (dep + 1)
            then let depot = {trefle = (dep + 1) ;
            pique = etat.depot.pique ;
            coeur = etat.depot.coeur ;
            carreau = etat.depot.carreau}
            in
     )etat.colonnes

     *)