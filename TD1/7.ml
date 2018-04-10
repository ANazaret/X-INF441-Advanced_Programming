let rec sum = function
	| [] -> 0
	| t::q -> t + sum q;;

let rec gsum f = 
	function 
		| [] -> 0
		| t::q -> f t + gsum f q;;

let sum l = gsum (function x->x) l;;

let rec zfilter = function
	| [] -> []
	| t::q when t>0 -> t::zfilter q
	| t::q -> zfilter q;;


let filter p = 	
	let rec aux = function
	| [] -> []
	| t::q when p t -> t::aux q
	| t::q -> aux q
	in aux
;;


let rec insert x = function
	| [] -> [x]
	| t::q when t>=x -> x::t::q
	| t::q -> t::insert x q;;      

let rec sort = function
	| [] -> []
	| t::q -> insert t (sort q);;

let rec insert leq x = function
	| [] -> [x]
	| t::q when leq x t -> x::t::q
	| t::q -> t::insert leq x q;;

let rec sort leq = function
	| [] -> []
	| t::q -> insert leq t (sort leq q);;


let rec sorted leq = function
	| [] -> true
	| t::[] -> true
	| a::b::q -> leq a b && sorted leq (b::q);;


let rec aux x = function
	| [] -> [[x]]
	| t::q -> (x::t::q)::(List.map (function l -> t::l) (aux x q));;

let rec perm = function
	| [] -> []
	| t::q -> (aux t q) @ (List.map (function l -> l@[t]) (perm q) );;

