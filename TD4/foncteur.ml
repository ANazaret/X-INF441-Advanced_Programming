
module type EqualityType =
	sig
		type t
		val equal : t -> t -> bool
	end
	
module Tree(ET : EqualityType) = 
	struct
		type element = ET.t
		type t = 
			| Empty
			| Node of t * element * t
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
		type t = T.t 
		let equal a1 a2 = T.equal a1 a2 
	end
)


let a1 = 	TT.Empty
