(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000

(* Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))


(** DESCRIPTION DE L'ALGORITHME DE GENERATION DES PERMUTATIONS

a) Créer tout d'abord les 55 premières paires suivantes:
  * premières composantes : 0 pour la premiere paire,
    puis ajouter 21 modulo 55 à chaque fois
  * secondes composantes : graine, puis 1, puis les "différences"
    successives entre les deux dernières secondes composantes.
    Par "différence" entre a et b on entend
      - Ou bien (a-b) si b<=a
      - Ou bien (a-b+randmax) si a<b

b) Trier ces 55 paires par ordre croissant selon leurs premières composantes,
   puis séparer entre les 24 premières paires et les 31 suivantes.
   Pour les 31 paires, leurs secondes composantes sont à mettre dans
   une FIFO f1_init, dans cet ordre (voir `Fifo.of_list` documenté dans
   `Fifo.mli`). De même pour les 24 paires, leurs secondes composantes sont
   à mettre dans une FIFO f2_init, dans cet ordre.

c) Un *tirage* à partir de deux FIFO (f1,f2) consiste à prendre
   leurs premières valeurs respectives n1 et n2 (cf `Fifo.pop`),
   puis calculer la "différence" de n1 et n2 (comme auparavant),
   nommons-la d. Ce d est alors le résultat du tirage, associé
   à deux nouvelles FIFO constituées des restes des anciennes FIFO
   auxquelles on a rajouté respectivement n2 et d (cf `Fifo.push`).

d) On commence alors par faire 165 tirages successifs en partant
   de (f1_init,f2_init). Ces tirages servent juste à mélanger encore
   les FIFO qui nous servent d'état de notre générateur pseudo-aléatoire,
   les entiers issus de ces 165 premiers tirages ne sont pas considérés.

e) La fonction de tirage vue précédemment produit un entier dans
   [0..randmax[. Pour en déduire un entier dans [0..limit[ (ou limit est
   un entier positif quelconque), on utilisera alors la fonction `reduce`
   fournie plus haut.
   Les tirages suivants nous servent à créer la permutation voulue des
   52 cartes. On commence avec une liste des nombres successifs entre 0 et 51.
   Un tirage dans [0..52[ nous donne alors la position du dernier nombre
   à mettre dans notre permutation. On enlève alors le nombre à cette position
   dans la liste. Puis un tirage dans [0..51[ nous donne la position
   (dans la liste restante) de l'avant-dernier nombre de notre permutation.
   On continue ainsi à tirer des positions valides dans la liste résiduelle,
   puis à retirer les nombres à ces positions tirées pour les ajouter devant
   la permutation, jusqu'à épuisement de la liste. Le dernier nombre retiré
   de la liste donne donc la tête de la permutation.

   NB: /!\ la version initiale de ce commentaire donnait par erreur
   la permutation dans l'ordre inverse).
*)

(*
A tail recursive implementation of List.combine
'a list -> 'b list -> ('a * 'b) list
*)
let combine list1 list2 =
  let rec loop list1 list2 acc =
    match (list1, list2) with
    | ([], []) -> List.rev acc
    | (x :: tail1, y :: tail2) -> loop tail1 tail2 ((x, y) :: acc)
    | _ -> invalid_arg "List.combine"
  in
  loop list1 list2 []

(*
    Créer tout d'abord les 55 premières paires suivantes:
  * premières composantes : 0 pour la premiere paire,
    puis ajouter 21 modulo 55 à chaque fois
  * secondes composantes : graine, puis 1, puis les "différences"
    successives entre les deux dernières secondes composantes.
    Par "différence" entre a et b on entend
      - Ou bien (a-b) si b<=a
      - Ou bien (a-b+randmax) si a<b
*)
let creerPaire tab1composant tab2composant n =
    let rec creerPaireAux tab1composant tab2composant n =
        if n = 55 then List.rev (combine tab1composant tab2composant)
        else let get2composant =
            match tab2composant with
            | b :: a :: tail -> if b <= a then a - b else a - b + randmax
            | _ -> failwith "erreur : tab2composant mal crée"
        in creerPaireAux
            ((((List.hd tab1composant) + 21) mod 55) :: tab1composant)
            (get2composant :: tab2composant) (n + 1)

    in creerPaireAux (21 :: 0 :: tab1composant) (1 :: n :: tab2composant) 2


(*
    b)Trier ces 55 paires par ordre croissant selon leurs premières composantes,
     puis séparer entre les 24 premières paires et les 31 suivantes.
     Pour les 31 paires, leurs secondes composantes sont à mettre dans
     une FIFO f1_init, dans cet ordre (voir `Fifo.of_list` documenté dans
     `Fifo.mli`). De même pour les 24 paires, leurs secondes composantes sont
     à mettre dans une FIFO f2_init, dans cet ordre.
*)
let rec getFifo acc n listeTri2comp =
    if n = 24 then Fifo.of_list listeTri2comp, Fifo.of_list (List.rev acc)
    else match listeTri2comp with
    | h :: t -> getFifo (h :: acc) (n + 1) t
    | _ -> failwith "erreur : listeTri2comp mal formée"



(*
c) Un *tirage* à partir de deux FIFO (f1,f2) consiste à prendre
   leurs premières valeurs respectives n1 et n2 (cf `Fifo.pop`),
   puis calculer la "différence" de n1 et n2 (comme auparavant),
   nommons-la d. Ce d est alors le résultat du tirage, associé
   à deux nouvelles FIFO constituées des restes des anciennes FIFO
   auxquelles on a rajouté respectivement n2 et d (cf `Fifo.push`).

d) On commence alors par faire 165 tirages successifs en partant
   de (f1_init,f2_init). Ces tirages servent juste à mélanger encore
   les FIFO qui nous servent d'état de notre générateur pseudo-aléatoire,
   les entiers issus de ces 165 premiers tirages ne sont pas considérés.
*)

(* Fifo -> Fifo -> (int * Fifo * Fifo) *)
let tirage f1 f2 =
    let (a,f1), (b,f2) = Fifo.pop f1, Fifo.pop f2
    in let value = if b <= a then a - b else a - b + randmax
    in value, (Fifo.push b f1), (Fifo.push value f2)

(*
renvoie f1 et f2 apres n tirage(s).
*)
let rec tirageDeFifo f1 f2 n =
    if n = 0 then f1, f2
    else let (_,f1,f2) = tirage f1 f2
    in tirageDeFifo f1 f2 (n - 1)

(*
e) La fonction de tirage vue précédemment produit un entier dans
   [0..randmax[. Pour en déduire un entier dans [0..limit[ (ou limit est
   un entier positif quelconque), on utilisera alors la fonction `reduce`
   fournie plus haut.
   Les tirages suivants nous servent à créer la permutation voulue des
   52 cartes. On commence avec une liste des nombres successifs entre 0 et 51.
   Un tirage dans [0..52[ nous donne alors la position du dernier nombre
   à mettre dans notre permutation. On enlève alors le nombre à cette position
   dans la liste. Puis un tirage dans [0..51[ nous donne la position
   (dans la liste restante) de l'avant-dernier nombre de notre permutation.
   On continue ainsi à tirer des positions valides dans la liste résiduelle,
   puis à retirer les nombres à ces positions tirées pour les ajouter devant
   la permutation, jusqu'à épuisement de la liste. Le dernier nombre retiré
   de la liste donne donc la tête de la permutation.
*)
let rec permut f1 f2 acc liste =
    let length = List.length liste in
    if length = 0 then acc
    else let d,f1,f2 = tirage f1 f2
    in let d_reduced = reduce d length
    in let valSortante = List.nth liste d_reduced
    in permut f1 f2
        (valSortante :: acc)
        (List.filter (fun x -> x != valSortante) liste)
(* renvoie la liste d'entiers de 1 à n *)
let getListInt n =
    let rec getListIntAux n acc =
        if n = -1 then acc
        else getListIntAux (n - 1) (n :: acc)
    in getListIntAux (n - 1) []

(*
A tail recursive implementation of List.split
('a * 'b) list -> 'a list * 'b list
*)
let split list =
  let rec loop list acc1 acc2 =
    match list with
    | [] -> (List.rev acc1, List.rev acc2)
    | (x, y) :: tail -> loop tail (x :: acc1) (y :: acc2)
  in
  loop list [] []

let shuffle n =
    let listePaire = creerPaire [] [] n in
    let listePaireTriee = List.fast_sort (fun (x1,_) (x2,_) -> compare x1 x2) listePaire in
    let (_ , listeTri2comp) = split listePaireTriee in
    let f1_init, f2_init = getFifo [] 0 listeTri2comp in
    let f1, f2 = tirageDeFifo f1_init f2_init 165 in
    permut f1 f2 [] (getListInt 52)
