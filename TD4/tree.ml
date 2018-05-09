type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

let a1 = 
	Node(
		Node(
			Empty,
			3,
			Node(
				Empty,
				5,
				Empty
			)
		),
		2,
		Node(
			Node(Empty,8,Empty),
			6,
			Node(
				Node(Empty,15,Empty),
				12,
				Empty
			)
		)
	)
				
let a2 = Node(Node(Empty,3,Empty),2,Empty)
let a3 = Node(Empty, 2, Node(Empty,3,Empty))

let rec elements = function
	| Empty -> []
	| Node(l,x,r) -> x::(elements l)@(elements r) 
				
				
let () =
  assert (elements a1 = [2; 3; 5; 6; 8; 12; 15])
	
	
let rec elements_append t xs = match t with
| Empty -> xs
| Node(l,x,r) -> x::(elements_append l (elements_append r xs))
  

let elements t = elements_append t [] 

let rec fold f s = function
	| Empty -> s
	| Node(l,x,r) -> fold f (fold f (f s x) l) r

let size = fold (fun x _ -> x+1) 0

let () =
  assert (size a1 = 7)
	
(*
Moins efficace :
 
let elements t = List.rev (fold (fun l x -> x::l) [] t)
*)

let equal a1 a2 = elements a1 = elements a2
let () =
  assert (equal a1 a1);
  assert (equal a2 a3);
  assert (not (equal a1 a2))



type 'a iterator = 'a tree list

let iterator t : 'a iterator = [t]

let rec next = function
	| [] -> None
	| Empty::q -> next q
	| Node(l,x,r)::q -> Some( x, l::r::q )  


let elements t =
	let rec aux = function
		| None -> []
		| Some(x,it) -> x::(aux (next it))   
	in aux (next (iterator t))
	
let () =
  assert (elements Empty = []);
  assert (elements a1 = [2; 3; 5; 6; 8; 12; 15]);
  assert (elements a2 = [2; 3]);
  assert (elements a3 = [2; 3])

let equal a1 a2 =
	let rec aux = function
		| (None,None) -> true
		| (None,_) -> false
		| (_,None) -> false
		| (Some(x1,it1), Some(x2,it2)) -> x1=x2 && aux (next it1, next it2)
	in aux (next (iterator a1), next (iterator a2))


let rec big_tree mini maxi =
	if mini > maxi then Empty else 
	if mini = maxi then Node(Empty,mini,Empty) else
		Node(big_tree (mini+1) ((mini+1+maxi)/2), mini, big_tree ((mini+1+maxi)/2+1) maxi)
	 
	
	
	

module type EqualityType =
	sig
		type t
		val equal : t -> t -> bool
	end
	
module Tree(ET : EqualityType) = 
	struct
		type element = ET.t
		type t = element tree
		type iterator = t list
		
		let iterator t : iterator = [t]
		
		let rec next (x:iterator)= match x with
    	| [] -> None
    	| Empty::q -> next q 
    	| Node(l,x,r)::q -> Some( x, (l::r::q : iterator) )  
	
		let rec equal t1 t2 = 
			let rec aux =  function
				| (None,None) -> true
    		| (None,_) -> false
    		| (_,None) -> false
    		| (Some(x1,it1), Some(x2,it2)) -> ET.equal x1 x2 && (aux (next it1, next it2))
    	in aux (next (iterator t1), next (iterator t2))
 
	
	end
	

module T = Tree( 
	struct 
		type t=int 
		let equal a1 a2 = a1 = a2 
	end
)


module TT = Tree( 
	struct 
		type t = int tree 
		let equal a1 a2 = equal a1 a2 
	end
)

let () =
  let a23 = Node (Empty, a2, Node (Empty, a3, Empty)) in
  let a32 = Node (Empty, a3, Node (Empty, a2, Empty)) in
  let a32' = Node (Node (Empty, a3, Empty), a2, Empty) in
  assert (TT.equal a23 a32);
  assert (TT.equal a23 a32')
	
	
	
	
type 'a iterator_infix = 
	| End
	| Consa of 'a * 'a iterator_infix
	| Consit of 'a tree * 'a iterator_infix
	
	
let iterator_infix t : 'a iterator_infix = 
	Consit(t,End)

let rec next = function
	| End -> None
	| Consa(a,q) -> Some(a,q)
	| Consit(Empty,q) -> next q
	| Consit(Node(l,x,r),q) -> next (Consit(l,Consa(x,Consit(r,q)))) 


let elements t =
	let rec aux = function
		| None -> []
		| Some(x,it) -> x::(aux (next it))   
	in aux (next (iterator_infix t))
	