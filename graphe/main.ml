type graph = {succ: int -> (int * int) list; size: int}

let succ = fun i ->
  match i with
  | 0 -> [(1, 1); (3, 1); (4, 1)]
  | 1 -> [(0, 1); (3, 1); (2, 1)]
  | 2 -> [(4, 1); (1, 1)]
  | 3 -> [(0, 1); (1, 1)]
  | 4 -> [(0, 1); (2, 1)]
  | _ -> failwith "dehors"
let g = {succ = succ; size = 5}

let to_adj_matrix (g: graph): int array array =
  let mat = Array.make_matrix g.size g.size 0 in
  for i = 0 to g.size - 1 do
    List.iter (fun (x, d) -> mat.(i).(x) <- d) (g.succ i)
  done;
  mat

let to_edges_list (g: graph): (int * int * int) list =
  let rec aux (current: int): (int * int * int) list =
    match current with
    | -1 -> []
    | n  -> (List.map (fun (x, d) -> (n, x, d)) (g.succ n)) @ (aux (n - 1))
  in aux (g.size - 1)

let dfs (g: graph) (s: int): int list =
  let visited = Array.make g.size false in
  let parcours = ref [] in
  let todo = Stack.create () in
  Stack.push s todo;
  while not (Stack.is_empty todo) do
    let current = Stack.pop todo in
    if not visited.(current) then begin
      visited.(current) <- true;
      parcours := current :: !parcours;
      List.iter (fun (x, _) -> Stack.push x todo) (g.succ current);
    end
  done;
  List.rev !parcours

let dfs_rec (g: graph) (s: int): int list =
  let visited = Array.make g.size false in
  let rec aux (s: int): int list =
    match visited.(s) with
    | true -> []
    | false -> visited.(s) <- true; s :: (List.concat_map (fun (x, _) -> aux x) (g.succ s))
  in aux s

let bfs (g: graph) (s: int): int list =
  let visited = Array.make g.size false in
  let parcours = ref [] in
  let todo = Queue.create () in
  Queue.push s todo;
  while not (Queue.is_empty todo) do
    let current = Queue.pop todo in
    if not (visited.(current)) then begin
      visited.(current) <- true;
      parcours := current :: !parcours;
      List.iter (fun (x, _) -> Queue.push x todo) (g.succ current)
    end
  done;
  List.rev !parcours

