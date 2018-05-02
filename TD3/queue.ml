type 'a queue = 'a list * 'a list

let empty :'a queue= ([],[])

let is_empty ((a,b) : 'a queue) = (a=[] && b=[])

let push x ((a,b):'a queue): 'a queue = (x::a,b) 

let rec pull ((a,b) as q: 'a queue) : ('a * 'a queue) option =
	if is_empty q then None else
	match b with
	| [] -> pull ([], List.rev a)
	| x::q -> Some(x, (a,q))