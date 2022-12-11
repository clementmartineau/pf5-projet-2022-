
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let game_to_string g = 
  match g with
  | Freecell -> "Freecell"
  | Seahaven -> "Seahaven"
  | Midnight -> "Midnight"
  | Baker -> "Baker"

(* J'ai trouvé ces 3 fonctions sur https://rosettacode.org/wiki/Read_a_file_line_by_line#OCaml *)
let input_line_opt ic =
    try Some (input_line ic)
    with End_of_file -> None

let read_lines ic =
    let rec aux acc =
        match input_line_opt ic with
        | Some line -> aux (line::acc)
        | None -> (List.rev acc)
    in aux []

let lines_of_file filename =
    let ic = open_in filename in
    let lines = read_lines ic in
    close_in ic;
    (lines)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  (* Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();*)
  (* print_string "C'est tout pour l'instant. TODO: continuer...\n"; *)
  let game = (game_to_string conf.game) in
  print_string ("\nJeu " ^ game ^ " avec la permutation " ^ (string_of_int conf.seed) ^ "\n");
  
  let etat = (Etat.etat_init (game_to_string conf.game) permut) in
  print_string (Etat.etat_to_string etat);
  
  let new_etat = 
    match config.mode with
    | Check(x) -> let filename = x in
                  let lines = lines_of_file (filename) in
                  let coups = (GameAction.creer_coup lines) in
                  (GameAction.check etat game coups 1)
    | _ -> (etat, (-1)) in

  if snd new_etat = -1 then exit 0;

  print_string ("\nAprès exécution des coups : \n");
  print_string (Etat.etat_to_string (fst new_etat));

  if snd new_etat = 0 then print_string "\nSUCCES\n"; 
  if snd new_etat = 0 then exit 0;
  
  if snd new_etat <> 0 then print_string ("\nECHEC " ^ string_of_int (snd new_etat) ^ "\n");
  exit 1

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
