(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (in 1..999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).
*)
val shuffle : int -> int list

(** split a list of pairs in a pair of lists *)
val split : ('a * 'b) list -> 'a list * 'b list

(** combine a pair of list in a list of pairs *)
val combine : 'a list -> 'b list -> ('a * 'b) list

(** draw a value from 2 fifo,
    adding the head of the second to the first Fifo,
    and the draw value to the the second Fifo.
*)
val tirage : int Fifo.t -> int Fifo.t -> int * int Fifo.t * int Fifo.t

(** return a list of int from 0 to n *)
val getListInt : int -> int list