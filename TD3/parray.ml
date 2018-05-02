type 'a node =
	| Array of 'a array
	| Delta of int * 'a * 'a parray

and 'a parray = 'a node ref

let rec norm ptab = match !ptab with
	| Array(a) -> ()
	| Delta(i,x,ptab2) -> 
		norm ptab2;
		match !ptab2 with 
		| Array(a) ->  let tmp = a.(i) in 
				begin
  				a.(i) <- x; 
  				ptab := Array(a);
  				ptab2 := Delta(i,tmp ,ptab);
				end  
		| _ -> assert false
			  
				
let rec lift f pa =
	norm pa;
	match !pa with 
	| Array(a) -> f a
	| _ -> assert false
	
let create n x : 'a parray= ref (Array(Array.make n x))

let init n f :'a parray = ref (Array(Array.init n f))

let rec length p = match !p with
	| Array(a) -> Array.length a
	| Delta(_,_,p2) -> length p2
 
let get a i = lift (fun a -> Array.get a i) a
let set pa i x : 'a parray = ref (Delta(i,x,pa))
let tolist pa = lift (Array.to_list) pa
let iter f = lift (Array.iter f)
let iteri f = lift (Array.iteri f)
let fold_left f x = lift (Array.fold_left f x)
let fold_right f a x = (lift (Array.fold_right f )) a x 


	