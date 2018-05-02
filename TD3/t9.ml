type t9 = 
	| Node of string * ((int * t9) list)

let empty = Node("",[])

let touche = function
	|'a' |'b' |'c' -> 2
	|'d' |'e' |'f' -> 3
	|'g' |'h' |'i' -> 4
	|'j' |'k' |'l' -> 5
	|'m' |'n' |'o' -> 6
	|'p' |'q' |'r' |'s' -> 7
	|'t' |'u' |'v' -> 8
	|'w' |'x' |'y' |'z' -> 9
	| _ -> -1

let insert s dico = match s with
	| "" -> dico
	| s -> let n = touche s.[0] in
				try
					let nextd = assoc n dico in 
						
				with Not_found -> ... 
					
	 