type ('a, 'b) automate = {
  sigma: 'a list;
  etats: 'b list;
  init: 'b list;
  final: 'b list;
  delta: ('b * 'a * 'b) list;
}

let a1 = {
  sigma = ['a'; 'b'];
  etats = List.init 3 (fun i -> i);
  init = [0];
  final = [2];
  delta = [(0, 'a', 1); (0, 'b', 2); (0, 'a', 2); (1, 'b', 2); (1, 'a', 0); (2, 'b', 0); (0, 'a', 0)];
}

let a2 = {
  sigma = ['a'];
  etats = [0; 1];
  init = [0];
  final = [1];
  delta = [(0, 'a', 1); (0, 'a', 0)];
}

let delta (auto: ('a, 'b) automate) (l: 'a) (q: 'b): 'b list =
  List.filter (fun x -> List.mem (q, l, x) auto.delta) auto.etats

let delta2 (auto) (l: 'a) (ql: 'b list): 'b list =
  let rec clean (l: 'c list): 'c list =
    match l with
    | [] -> []
    | x :: l' when List.mem x l' -> clean l'
    | x :: l' -> x :: (clean l')
  in List.concat_map (fun q -> delta auto l q) ql
    |> clean
    |> List.sort compare

let rec delta_extend (auto: ('a, 'b) automate) (w: 'a list) (q: 'b list): 'b list =
  match w with
  | [] -> q
  | l :: w' -> List.concat_map (fun q' -> delta_extend auto w' [q']) (List.concat_map (delta auto l) q)

let accept (auto: ('a, 'b) automate) (w: 'a list): bool =
  delta_extend auto w auto.init
  |> List.exists (fun qf -> List.mem qf auto.final)

let complete (auto: ('a, 'b) automate): ('a, 'b option) automate =
  let etats' = None :: (List.map (fun x -> Some x) auto.etats) in
  let init' = List.map (fun x -> Some x) auto.init in
  let final' = List.map (fun x -> Some x) auto.final in
  let delta' = List.concat_map (fun q ->
    List.concat_map (fun l ->
      let next = delta auto l q in if next = []
      then [(Some q, l, None)]
      else List.map (fun q' ->
        (Some q, l, Some q'))
      next)
    auto.sigma)
  auto.etats
  in {
    sigma = auto.sigma;
    etats = etats';
    init = init';
    final = final';
    delta = delta' @ (List.map (fun l -> (None, l, None)) auto.sigma);
  }

let determinise (auto: ('a, 'b) automate): ('a, 'b list) automate =
  let rec parties (l: 'b list): 'b list list =
    match l with
    | [] -> [[]]
    | q :: l' -> (parties l') @ (List.map (fun e -> q :: e) (parties l'))
  in
  let etats = parties auto.etats in
  let init = List.map (fun q -> [q]) auto.init in
  let final = List.filter (fun p -> List.exists (fun q -> List.mem q p) auto.final) etats in
  let delta = List.concat_map (fun q -> List.map (fun l -> (q, l, delta2 auto l q)) auto.sigma) etats in
  {
    sigma = auto.sigma;
    etats = etats;
    init = init;
    final = final;
    delta = delta;
  }

let complementaire (auto: ('a, 'b) automate): ('a, 'b option) automate =
  let complet = complete auto in
  let final = List.filter (fun q -> not (List.mem q complet.final)) complet.etats in
  {complet with final = final}

let ( * ) (auto1: ('a, 'b) automate) (auto2: ('a, 'c) automate): ('a, ('b * 'c)) automate =
  let etats = List.concat_map (fun q1 -> List.map (fun q2 -> (q1, q2)) auto2.etats) auto1.etats in
  let init = List.concat_map (fun q1 -> List.map (fun q2 -> (q1, q2)) auto2.init) auto1.init in
  let final = List.concat_map (fun q1 -> List.map (fun q2 -> (q1, q2)) auto2.final) auto1.final in
  let delta = List.concat_map (fun (q1, l, q1') -> List.filter_map (fun (q2, l', q2') -> if l = l' then Some ((q1, q2), l, (q1', q2')) else None) auto2.delta) auto1.delta in
  {sigma = auto1.sigma; etats = etats; init = init; final = final; delta = delta}

let intersect (auto1: ('a, 'b) automate) (auto2: ('a, 'c) automate): ('a, ('b * 'c)) automate =
  let auto = auto1 * auto2 in auto

let union (auto1: ('a, 'b) automate) (auto2: ('a, 'c) automate): ('a, ('b * 'c)) automate =
  let auto = auto1 * auto2 in
  let final1 = List.concat_map (fun q1 -> List.map (fun q2 -> (q1, q2)) auto2.final) auto1.etats in
  let final2 = List.concat_map (fun q1 -> List.map (fun q2 -> (q1, q2)) auto2.etats) auto1.final in
  {auto with final = final1 @ final2}
