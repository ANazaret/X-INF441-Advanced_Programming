type 'a tree = 
	 Node of int * 'a * int * 'a tree list


(*	{
		priority: int; 
		value: 'a;
		children: 'a heap list;
		rank: int  
	}
 *)


let node p x = Node(p,x,0,[])

let rec size = function Node(_,_,_,l) -> 1 + size_list l
and size_list = function
	| [] -> 0
	| t::q -> size t + size_list q


let rank (Node(p,x,r,t)) = r
(*
	let rec len_list res = function
		| [] -> res
		| t::q -> len_list (1+res) q
	in len_list 0 t
	*)

let rec link (Node(p, x, r, l) as t) (Node(q, y, s, m) as u) = 
	assert (rank t = rank u); 
	if p<=q 
	then	Node(p, x, r+1, u:: l)
	else  link u t
	
	
let root_priority = function
	| Node(p,_,_,_) -> p

let root_value = function
	| Node(_,x,_,_) -> x

let () =
  let zero = node 0 "a" in
  let one = link zero zero in
  let two = link one one in
  assert (size two = 4)


let () =
  let zero = node 0 "a" in
  let one = link zero zero in
  let two = link one one in
  assert (rank two = 2) 
	

