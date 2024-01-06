type lit = char * bool
type cld = lit list
type fnc = cld list

let fnc_true = []

let f: fnc = [[('a', true); ('b', false)]; [('c', false); ('a', false)]; [('c', true); ('b', false)]]

let print_cld (cl: cld): unit =
  Printf.printf "(%s)" (String.concat ", " (List.map (fun (v, c) -> let p = if c then "" else "-" in Printf.sprintf "%s%c" p v) cl))

let print_fnc (f: fnc): unit =
  Printf.printf "["; List.iter print_cld f; Printf.printf "]\n"

let rec vars (f: fnc): char list =
  match f with
  | [] -> []
  | cl :: f' -> (match cl with
                 | [] -> vars f'
                 | (c, v) :: cl' -> c :: vars (cl' :: f'))

let clean_vars (f: fnc): char list =
  let rec aux (l: char list): char list =
    match l with
    | [] -> []
    | c :: l' when List.mem c l' -> aux l'
    | c :: l' -> c :: (aux l')
  in
  aux (vars f)

(* Retourne None si la clause est satisfaite et Some c sinon *)
let rec cld_assume (cl: cld) (var: char) (value: bool): cld option =
  match cl with
  | [] -> Some []
  | (l, v) :: cl' when l = var -> if v = value then None else (cld_assume cl' var value)
  | l :: cl' -> (match cld_assume cl' var value with
                 | None -> None
                 | Some a -> Some (l :: a))

let rec assume (f: fnc) (var: char) (value: bool): fnc =
  match f with
  | [] -> []
  | cl :: f' -> (match cld_assume cl var value with
                 | None -> assume f' var value
                 | Some c -> c :: (assume f' var value))

let rec quine (f: fnc): bool =
  if f = [] then true
  else if List.mem [] f then false
  else match clean_vars f with
       | [] -> false
       | p :: var' -> quine (assume f p true) || quine (assume f p false)


