open Card
open List

type depot = {trefle : int; pique : int; coeur : int; carreau : int}
type colonnes = (card list) FArray.t
type registres = ((card option) FArray.t) option
type etat = {depot : depot; colonnes : colonnes; registres : registres; historique : string list}

(* "depot_init" initialise un dépôt vide *)
let depot_init = {trefle = 0; pique = 0; coeur = 0; carreau = 0}

(* "depot_to_string" transforme un dépot en string pour pouvoir l'afficher *)
let depot_to_string depot =
    "\nDépôt :\n- " ^
    string_of_int depot.trefle ^ " trèfle\n- " ^
    string_of_int depot.pique ^ " pique\n- " ^ 
    string_of_int depot.coeur ^ " coeur\n- "^
    string_of_int depot.carreau ^ " carreau\n\n"

(* "remplir_une_colonne" distribue les cartes de la permutation dans une colonne *)
let rec remplir_une_colonne colonne n p =
    if (n = 0) then (colonne, p)
    else remplir_une_colonne ((of_num (hd p)) :: colonne) (n-1) (tl p)

(* "une_colonne_to_string" transforme une colonne en string pour pouvoir l'afficher *)
let rec une_colonne_to_string colonne =
    match colonne with
    | [] -> ""
    | h :: t -> une_colonne_to_string t ^ " " ^ (to_string h)  

(* "remplir_colonnes" distribue les cartes de la permutation dans toutes les colonnes *)
let remplir_colonnes colonnes permut f n =
    let rec aux colonnes i n permut =
        if(i = n)
            then (colonnes, permut)
        else
            let col = remplir_une_colonne (FArray.get colonnes i) (f i) permut in
            let cols = FArray.set colonnes i (fst col) in
            aux cols (i+1) n (snd col)
    in aux colonnes 0 n permut

(* Dans le cas ou le jeu est Baker's Dozen, "descendre_rois" met tous les rois au fond d'une colonne *)
let descendre_rois colonne =
    let rec aux roi autres c = 
        match c with
        | [] -> rev autres @ rev roi
        | h :: t -> if fst h = 13 then aux (h :: roi) autres t
                    else aux roi (h :: autres) t
    in aux [] [] colonne

(* "colonnes_init" initialise les colonnes en fonction du jeu *)
let colonnes_init game permut = 
    match game with
    | "Freecell" -> remplir_colonnes (FArray.make 8 []) permut (fun i -> if i mod 2 = 0 then 7 else 6) 8
    | "Seahaven" -> remplir_colonnes (FArray.make 10 []) permut (fun i -> 5) 10
    | "Midnight" -> remplir_colonnes (FArray.make 18 []) permut (fun i -> if i < 17 then 3 else 1) 18
    | "Baker" -> let c = remplir_colonnes (FArray.make 13 []) permut (fun i -> 4) 13
                in ((FArray.map descendre_rois (fst c)), (snd c))
    | _ -> failwith "Invalid Game"

(* "colonnes_to_string" transforme toutes les colonnes en string pour pouvoir les afficher *)
let colonnes_to_string colonnes =
    let rec aux colonnes n =
        if(n = FArray.length colonnes) then "\n"
        else "Colonne n°" ^ string_of_int (n+1) ^ " :" ^ une_colonne_to_string (FArray.get colonnes n) ^ "\n" ^ aux colonnes (n+1) 
    in aux colonnes 0

(* "remplir_registres" distribue les cartes restantes de la permutation dans les registres *)
let remplir_registres registres n permut =
    let rec aux registres permut n i =
        if n = i then registres
        else aux (FArray.set (registres) i (Some(of_num(hd permut)))) (tl permut) n (i+1)
    in aux registres permut n 0

(* "registres_init" initialise les registres en fonction du jeu *)
let registres_init game permut =
    match game with
    | "Freecell" -> Some (FArray.make 4 None)
    | "Seahaven" -> Some (remplir_registres (FArray.make 4 None) 2 permut)
    | _ -> None

(* "registres_to_string" transforme les registres en string pour pouvoir les afficher *)
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

(* "etat_make" creer un nouvel etat contenant le dépôt, les colonnes et les registres demandés *)
let etat_make depot colonnes registres historique = {depot = depot; colonnes = colonnes; registres = registres; historique = historique}

(* "etat_init" initialise un etat en fonction du jeu *)
let etat_init game permut =
let col = colonnes_init game permut in {depot = depot_init ; colonnes = fst col; registres = registres_init game (snd col); historique = []}

(* "etat_to_string" transforme un etat en string pour pouvoir l'afficher *)
let etat_to_string etat = 
    depot_to_string etat.depot ^ colonnes_to_string etat.colonnes ^ registres_to_string etat.registres


(* renvoie le score d'un état *)
let get_score etat =
    etat.depot.trefle + etat.depot.coeur + etat.depot.pique + etat.depot.carreau


(* test d'égalité registre. Si = -> 0 sinon -> 1 *)
let compare_registres registre1 registre2 =
    match registre1, registre2 with
    | Some(r1),Some(r2) -> FArray.compare_card r1 r2
    | None,None -> 0
    | _ -> 1

(* pour comparer les colonnes (contenant des FArray), on a implémenté une fonction compare *)
(* test d'égalité colonnes. Si = -> 0 sinon -> 1 *)
let compare_colonnes cols1 cols2 =
    FArray.compare_cols cols1 cols2

(* comparaison entre états *)

(*
let compare_etat etat1 etat2 =
  let res = Stdlib.compare etat1.registres etat2.registres in
  if (res = 0) then Stdlib.compare etat1.colonnes etat2.colonnes else comp
  *)



let compare_etat etat1 etat2 =
    (* On commence par vérifier l'égalité au score *)
    if get_score etat1 > get_score etat2 then 1
    else if get_score etat1 < get_score etat2 then -1
    (* si score = on vérifie l'égalité parfaite *)
    else if compare_registres etat1.registres etat2.registres = 0 && compare_colonnes etat1.colonnes etat2.colonnes = 0 then 0
    (* si pas égalité parfaite on compare les dépots *)
    else let c = Stdlib.compare etat1.registres etat2.registres in
         if (c = 0) then Stdlib.compare etat1.colonnes etat2.colonnes
         else c