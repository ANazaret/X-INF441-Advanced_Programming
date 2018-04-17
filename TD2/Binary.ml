type binary = int list

let rec to_int = function
	| [] -> 0
	| t::q -> 1 lsl t + to_int q


let of_int n =
	let rec aux shift = function
  	| 0 -> []
  	| x when (x mod 2 = 1) -> shift::(aux (shift+1) (x/2))
  	| x -> aux (shift+1) (x/2)
	in aux 0 n

let add_bit b k =
	let rec aux k = function
		| [] -> [k]
		| t::q when t<k -> t::(aux k q)
		| t::q when t=k -> aux (k+1) q
		| t::q -> k::t::q
  in aux k b

let rec add b1 = function
	| [] -> b1
	| t::q -> add (add_bit b1 t) q 
	
	

	
let () =
  assert (to_int [0;1;4] = 19);
  assert (of_int 19 = [0;1;4]);
	assert (add_bit [0;1;4] 3 = [0;1;3;4]);
  assert (add_bit [0;1;4] 5 = [0;1;4;5]);
  assert (add_bit [0;1;4] 0 = [2;4]);
	
	let add_int x y = to_int (add (of_int x) (of_int y)) in
    assert (add_int 19 33 = 52);
    assert (add_int 41 29 = 70);
    assert (add_int 33 85 = 118);
	
	
	