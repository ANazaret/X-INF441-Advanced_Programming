open BinomialTree

type 'a heap = 'a tree list

let empty = []

let add_tree h t  =
	let rec aux t = 
		let rt = rank t in function
		| [] -> [t]
		| a::q when (rank a)<rt -> t::(aux t q)
		| a::q when rt = rank a -> aux (link a t) q
		| a::q -> t::a::q
  in aux t h
	
let insert h p x = add_tree h (node p x)

let find_min h = 
	let rec aux (res,pmin) = function
		| [] -> res
		| t::q when root_priority t <= pmin -> 
			aux (root_value t, root_priority t) q
		| t::q ->aux (res,pmin) q
	in match h with
	| [] -> assert false
	| t::q -> aux (root_value t, root_priority t) q



let () =
  let h = empty in
  let h = insert h 5 "a" in
  assert (find_min h = "a");
  let h = insert h 8 "b" in
  assert (find_min h = "a");
  let h = insert h 2 "c" in
  assert (find_min h = "c");
  let h = insert h 2 "d" in
  assert (find_min h = "c");
  let h = insert h 2 "e" in
  assert (find_min h = "c")