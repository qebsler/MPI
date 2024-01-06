let rec fusion (t1: int list) (t2: int list): int list =
  match t1, t2 with
  | [], _ -> t2
  | _, [] -> t1
  | h1 :: _, h2 :: t' when h1 > h2 -> h2 :: fusion t1 t'
  | h1 :: t1', _ -> h1 :: fusion t1' t2

let rec sub (t: int list) (l: int): int list * int list =
  match t with
  | [] -> ([], [])
  | h :: q when l > 0 -> let l1, l2 = sub q (l - 1) in (h :: l1, l2)
  | h :: q -> ([], t)

let rec mergesort (l: int list): int list =
  let mid = List.length l / 2 in
  match sub l mid with
  | [], [] -> []
  | [], [a] | [a], [] -> [a]
  | l1, l2 -> fusion (mergesort l1) (mergesort l2)


