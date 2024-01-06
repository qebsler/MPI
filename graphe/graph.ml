type graph = int list array
(** Graphe sous forme de liste d'adjacence *)

let g1: graph = [|
  [1];
  [2; 3];
  [5];
  [0];
  [3; 5];
  [2; 4];
|]

let g2: graph = [|
  [1; 4];
  [5];
  [5];
  [1; 4];
  [2; 6];
  [7];
  [0];
  [3; 4; 8];
  []
|]

let g3: graph = [|
  [1];
  [2; 3];
  [4];
  [];
  [3; 5];
  [2]
|]

let g4: graph = [|
  [2; 1];
  [2; 3];
  [0; 7];
  [7; 4];
  [3; 5; 6];
  [];
  [5];
  [6; 5];
|]

(** Graphe biparti *)
let g5: graph = [|
  [1; 3];
  [0; 4; 2];
  [5; 1; 3];
  [2; 0];
  [1; 5];
  [2; 4]
|]

let size = Array.length

let dfs (g: graph) (s: int) (visited: bool array) (l: int list): int list =
  let todo = Stack.create () in
  let rec aux (st: int Stack.t): int list =
    if Stack.is_empty st then l
    else begin
      let s = Stack.pop st in
      visited.(s) <- true;
      List.iter (fun x -> if not visited.(x) then Stack.push x st) g.(s);
      s :: aux st
    end
  in
  Stack.push s todo;
  aux todo

let bfs (g: graph) (s: int): int list =
  let visited = Array.make (size g) false in
  let todo = Queue.create () in
  let rec aux (todo: int Queue.t): int list =
    if Queue.is_empty todo then []
    else begin
      let s = Queue.pop todo in
      visited.(s) <- true;
      List.iter (fun x -> if not visited.(x) then Queue.push x todo) g.(s);
      s :: aux todo
    end
  in
  Queue.push s todo;
  aux todo

let prefix (g: graph) (s: int) (visited: bool array) (l: int list): int list =
  let todo = Stack.create () in
  let rec aux (todo: (int * (int list)) Stack.t): int list =
    if Stack.is_empty todo then l
    else begin
      let s, neighboor = Stack.pop todo in
      match neighboor with
      | [] -> s :: (aux todo)
      | t :: n' -> Stack.push (s, n') todo;
        if not visited.(t) then begin
          visited.(t) <- true;
          Stack.push (t, g.(t)) todo;
        end;
        aux todo
    end
  in
  Stack.push (s, g.(s)) todo;
  aux todo

(** FIXME: Doublons *)
let tri_prefixe (g: graph): int list =
  let n = size g in
  let l = ref [] in
  let visited = Array.make n false in
  for i = 0 to n - 1 do
    if not visited.(i) then l := prefix g i visited !l;
  done;
  !l

let pred (g: graph) (s: int): int list =
  let l, _ = Array.fold_left
    (fun (acc, i) x -> if List.mem s x then (i :: acc, i + 1) else (acc, i + 1))
    ([], 0) g
  in l

let transpose (g: graph): graph =
  let n = size g in
  Array.init n (fun i -> pred g i)

let parcours (g: graph) (regen: int list): int list list =
  let visited = Array.make (size g) false in
  let l = ref [] in
  let i = ref 0 in
  while Array.mem false visited && !i < size g do
    let s = List.nth regen !i in
    if not visited.(s) then begin l := (dfs g s visited []) :: !l end;
    incr i;
  done;
  !l

(** FIXME: doublons & mauvaises cfc dans les cas singletons *)
let cfc (g: graph): int list list =
  let t = tri_prefixe (transpose g) in
  parcours g t

let print_tab (t: 'a array) (printer: 'a -> string): unit =
  Array.iteri (fun i x -> Printf.printf "%d: %s; " i (printer x)) t;
  Printf.printf "\n";
  flush stdout

let printer_option (s: bool option): string =
  match s with
  | Some c -> Printf.sprintf "%b" c
  | None -> Printf.sprintf "None"

let is_bipartite (g: graph): bool =
  (** Colorie le graphe en faisant un parcours en profondeur *)
  let colors = Array.make (size g) None in
  let rec color (visited: bool array) (todo: (int * bool) list): bool =
    match todo with
    | [] -> true
    (* Amélioration possible en supprimant la variable visited et en considérant que None est pas visité *)
    | (s, c) :: todo' when not visited.(s) -> visited.(s) <- true;
        colors.(s) <- Some c;
        let queue = List.fold_left (fun acc x -> (x, not c) :: acc) todo' g.(s) in
        color visited queue
    | (s, c) :: todo' -> match colors.(s) with
                         | None -> raise Not_found
                         | Some c' -> c = c' && (color visited todo')
  in
  let visited = Array.make (size g) false in
  let todo = [(0, true)] in
  color visited todo
