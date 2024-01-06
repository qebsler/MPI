let print_tab (t: int array): unit =
  Array.iter (Printf.printf "%d ") t; Printf.printf "\n"

let swap (t: int array) (i: int) (j: int): unit =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp

let partition (t: int array) (g: int) (d: int) (p: int): int =
  let i = ref g and j = ref g in
  swap t p d;
  while !i < d do
    if t.(!i) > t.(d) then incr i
    else begin
      swap t !i !j;
      incr i;
      incr j;
    end;
  done;
  swap t !j d;
  !j

let quick_sort (t: int array): unit =
  let rec aux (g: int) (d: int): unit =
    if g < d then begin
      let p = (Random.int (d - g + 1)) + g in
      let j = partition t g d p in
      aux g (j - 1);
      aux (j + 1) d;
    end
  in aux 0 (Array.length t - 1)
