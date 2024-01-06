type joueur = A | B
type 'a etat = 'a * joueur
type 'a graph = 'a etat -> 'a etat list
type 'a jeu = {succ: 'a graph; start: 'a etat; win: 'a etat -> joueur -> bool}
type 'a strategie = 'a etat -> 'a etat
type int_e = I of int | Minf | Pinf
type 'a heuristique = 'a etat -> joueur -> int_e
type 'a mm_tree =
  | Leaf of 'a etat * int_e
  | Node of 'a etat * 'a mm_tree list
type 'a mm_tree_completed =
  | LeafC of 'a etat * int_e
  | NodeC of 'a etat * int_e * 'a mm_tree_completed list

exception Terminal_state

(** Retourne l'autre joueur *)
let not = function
  | A -> B
  | B -> A

(** Fonctions de comparaison sur les entiers étendus *)
let compare (a: int_e) (b: int_e): int =
  match a, b with
  | I e, I f -> compare e f
  | Minf, Minf | Pinf, Pinf -> 0
  | Minf, _ | _, Pinf -> -1
  | Pinf, _ | _, Minf -> 1

let (>=) = fun a b -> compare a b >= 0
let (<=) = fun a b -> compare a b <= 0
let min = fun a b -> if a <= b then a else b
let max = fun a b -> if a <= b then b else a

(** Algorithme de minmax basé sur l'heuristique [h] avec une profondeur [d] *)
let minmax (game: 'a jeu) (h: 'a heuristique) (j: joueur) (e: 'a etat) (d: int): int_e =
  let rec mm (e: 'a etat) (d: int): int_e =
    if game.succ e = [] then
      match e with
      | e when game.win e j -> Pinf
      | e when game.win e (not j) -> Minf
      | e -> I 0
    else if d = 0 then h e j
    else begin
      let s, je = e in
      let cmp, init = if je = j then max, Minf else min, Pinf in
      List.fold_left (fun acc e' -> cmp acc (mm e' (d - 1))) init (game.succ e)
    end
  in
  mm e d

(** Algorithme de minmax avec élaguage alpha-beta (plus performant que le minmax sans élaguage) *)
let minmax_ab (game: 'a jeu) (h: 'a heuristique) (j: joueur) (e: 'a etat) (d: int): int_e =
  let exception Break of int_e in
  let rec mm (e: 'a etat) (d: int) (alpha: int_e) (beta: int_e): int_e =
    if game.succ e = [] then
      match e with
      | e when game.win e j -> beta
      | e when game.win e (not j) -> alpha
      | e when beta <= I 0 -> beta
      | e when alpha >= I 0 -> alpha
      | e -> I 0
    else if d = 0 then
      match h e j with
      | h' when h' >= beta -> beta
      | h' when h' <= alpha -> alpha
      | h' -> h'
    else begin
      let _, je = e in
      if je = j then begin
        let v = ref alpha in
        try (List.iter (fun e' -> v := max !v (mm e' (d - 1) !v beta); if !v >= beta then raise (Break beta))) (game.succ e); !v with
        | Break p -> p
      end else begin
        let v = ref beta in
        try (List.iter (fun e' -> v := min !v (mm e' (d - 1) alpha !v); if !v <= alpha then raise (Break alpha)) (game.succ e); !v) with
        | Break p -> p
      end
    end
  in mm e d Minf Pinf

(** Calcul l'arbre des états successifs pour un minmax avec un arbre *)
let make_mm_tree (game: 'a jeu) (h: 'a heuristique) (j: joueur) (start: 'a etat) (deepth: int): 'a mm_tree =
  let rec make_tree (e: 'a etat) (deepth: int): 'a mm_tree =
    if game.succ e = [] then
      (if game.win e j then
        Leaf (e, Pinf)
      else if game.win e (not j) then
        Leaf (e, Minf) else Leaf (e, I 0))
    else if deepth = 0
    then Leaf (e, h e j)
    else Node (e, List.map (fun e' -> make_tree e' (deepth - 1)) (game.succ e))
  in make_tree start deepth

(** Complète l'arbre de minmax en remontant les valeurs grâce à un minmax *)
let rec complete_mm_tree (tr: 'a mm_tree) (j: joueur): 'a mm_tree_completed =
  let fold_aux (min_max: bool) (acc: int_e) (tr: 'a mm_tree_completed): int_e =
    let cmp = if min_max then min else max in
    match tr with
    | LeafC (_, h) -> cmp acc h
    | NodeC (_, h, _) -> cmp acc h
  in
  match tr with
  | Leaf (e, h) -> LeafC (e, h)
   (* Cas ou l'on cherche le max *)
  | Node ((etat, j_etat) as e, succ) when j = j_etat -> let succ' = List.map (fun x -> complete_mm_tree x j) succ in
      NodeC (e, List.fold_left (fold_aux false) Minf succ', succ')
   (* Cas ou l'on cherche le min *)
  | Node (e, succ) -> let succ' = List.map (fun x -> complete_mm_tree x j) succ in
      NodeC (e, List.fold_left (fold_aux true) Pinf succ', succ')

(** Complete et élague l'arbre de minmax avec l'algorithme de minmax avec élaguage alpha-beta *)
let lazy_complete_mm_tree (tr: 'a mm_tree) (j: joueur): 'a mm_tree_completed =
  let rec fold_max ((v_acc, tr_acc) as acc: int_e * 'a mm_tree_completed list) (beta: int_e) (succ: 'a mm_tree list): int_e * 'a mm_tree_completed list =
    match succ with
    | [] -> acc
    | Node (e, s) :: succ' -> let node = complete (Node (e, s)) v_acc beta in
        (match node with
        | LeafC (e, h) when h >= beta -> (beta, LeafC (e, beta) :: tr_acc)
        | NodeC (e, h, succ'') when h >= beta -> (beta, NodeC (e, beta, succ'') :: tr_acc)
        | LeafC (e, h) -> fold_max (max v_acc h, LeafC (e, h) :: tr_acc) beta succ'
        | NodeC (e, h, succ'') -> fold_max (max v_acc h, NodeC (e, h, succ'') :: tr_acc) beta succ')
    | Leaf (e, h) :: _ when h >= beta -> (beta, LeafC (e, beta) :: tr_acc)
    | Leaf (e, h) :: succ' -> fold_max (max h v_acc, LeafC (e, h) :: tr_acc) beta succ'

  and fold_min ((v_acc, tr_acc) as acc: int_e * 'a mm_tree_completed list) (alpha: int_e) (succ: 'a mm_tree list): int_e * 'a mm_tree_completed list =
    match succ with
    | [] -> acc
    | Node (e, s) :: succ' -> let node = complete (Node (e, s)) alpha v_acc in
        (match node with
        | LeafC (e, h) when h <= alpha -> (alpha, LeafC (e, alpha) :: tr_acc)
        | NodeC (e, h, s) when h <= alpha -> (alpha, NodeC (e, alpha, s) :: tr_acc)
        | LeafC (e, h) -> fold_min (min v_acc h, LeafC (e, h) :: tr_acc) alpha succ'
        | NodeC (e, h, s) -> fold_min (min v_acc h, NodeC (e, h, s) :: tr_acc) alpha succ')
    | Leaf (e, h) :: _ when h <= alpha -> (alpha, LeafC (e, alpha) :: tr_acc)
    | Leaf (e, h) :: succ' -> fold_min (min h v_acc, LeafC (e, h) :: tr_acc) alpha succ'

  and complete (tr: 'a mm_tree) (alpha: int_e) (beta: int_e): 'a mm_tree_completed =
    match tr with
    | Leaf (e, h) when beta <= h -> LeafC (e, beta)
    | Leaf (e, h) when alpha >= h -> LeafC (e, alpha)
    | Leaf (e, h) -> LeafC (e, h)
    (* Cas de maximisation *)
    | Node ((etat, j_etat) as e, succ) when j_etat = j -> let v, succ' = fold_max (alpha, []) beta succ in NodeC (e, v, succ')
    (* Cas de minimisation *)
    | Node (e, succ) -> let v, succ' = fold_min (beta, []) alpha succ in NodeC (e, v, succ')
  in
  complete tr Minf Pinf

(** Calcul la taille d'un arbre *)
let rec size (t: 'a mm_tree_completed): int =
  match t with
  | LeafC (_, _) -> 1
  | NodeC (_, _, succ) -> List.fold_left (fun acc s -> acc + (size s)) 1 succ

(** Retourne la stratégie induite par l'heuristique en calculant l'arbre de minmax élagué *)
let strat_heuristique (game: 'a jeu) (h: 'a heuristique) (j: joueur) (deepth: int): 'a strategie =
  let finder (expected_value: int_e) (x: 'a mm_tree_completed): bool =
    match x with
    | LeafC (_, v) -> v = expected_value
    | NodeC (_, v, _) -> v = expected_value
  in
  fun e ->
    let tree = make_mm_tree game h j e deepth in
    let c_tree = lazy_complete_mm_tree tree j in
    match c_tree with
    | LeafC (_, _) -> raise Terminal_state
    | NodeC (e, v, succ) ->
        match List.find (finder v) succ with
        | LeafC (e, _) -> e
        | NodeC (e, _, _) -> e

(** Fonction testant l'égalité de deux ensembles représentés par des listes *)
let (=-=) (l: 'b list) (l': 'b list): bool =
  (List.for_all (fun x -> List.mem x l') l) && (List.for_all (fun x -> List.mem x l) l')

(** Calcul l'intersection de deux ensembles représentés par des listes *)
let rec (^^) (l1: 'a etat list) (l2: 'a etat list): 'a etat list =
  match l1 with
  | [] -> []
  | e :: l1' -> if List.mem e l2 then e :: (l1' ^^ l2) else l1' ^^ l2

(** Retourne l'attracteur initial et la fonction représentant la relation de récurence entre les attracteurs *)
let attracteur_transition (game: 'a jeu) (etats: 'a etat list) (j: joueur) =
  let v_j, v_notj = List.filter (fun e -> game.succ e <> []) etats
        |> List.partition (fun (_, je) -> je = j) in
  let following_attr (prec: 'a etat list): 'a etat list =
    let j = List.filter (fun s_a -> List.exists (fun s_b -> List.mem s_b prec) (game.succ s_a)) v_j in
    let not_j = List.filter (fun s_b -> List.for_all (fun s_a -> List.mem s_a prec) (game.succ s_b)) v_notj in
    j @ not_j @ prec
  in
  let a0 = List.filter (fun e -> game.win e j) etats in
  a0, following_attr

(** Calcul les états attracteurs d'un joueur *)
(* La terminaison est assuré par le fait que etats est une liste finie *)
let attracteur (game: 'a jeu) (etats: 'a etat list) (j: joueur): 'a etat list =
  let j0, transition = attracteur_transition game etats j in
  let rec point_fixe (prec: 'a etat list): 'a etat list =
    let j_next = transition prec in
    if j_next =-= prec then prec else point_fixe j_next
  in point_fixe j0

(** Retourne vrai si le joueur [j] pssède une stratégie gagnante *)
let is_win (game: 'a jeu) (etats: 'a etat list) (j: joueur): bool =
  let attr = attracteur game etats j in
  List.mem game.start attr

(** Retourne la stratégie induite par les attracteurs *)
let strat_attracteur (game: 'a jeu) (etats: 'a etat list) (j: joueur): 'a strategie =
  let attracteur_j = attracteur game etats j in
  fun sj ->
    let succ = game.succ sj in
    if List.mem sj attracteur_j
    then List.hd (succ ^^ (attracteur_j))
    else if succ = [] then raise Terminal_state else List.hd succ

(** Retourne la représentation du jeu des allumettes à [i] allumettes *)
let jeu_allu (i: int) (j: joueur): int jeu =
  let g: int graph = fun (i, j) ->
    match i with
    | 0 | 1 -> []
    | 2 -> [(1, not j)]
    | 3 -> [(1, not j); (2, not j)]
    | i -> [(i - 1, not j); (i - 2, not j); (i - 3, not j)]
  in
  let win ((e, je): int etat) (j: joueur): bool = (e = 1 && not j = je) in
  {succ = g; win = win; start = (i, j)}

let h: int heuristique = fun (i, je) j -> I 0
let game = jeu_allu 13 A
let states: int etat list = List.init 13 (fun i -> (i + 1, A)) @ List.init 13 (fun i -> (i + 1, B))
