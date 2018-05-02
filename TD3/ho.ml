let rec somme = function
	| 0 -> 0
	| n -> somme (n -1) + n

let rec somme2 = function 
	| 0 -> 0
	| n -> somme2 (n -1) + n*n

let sommef f n = 
	let rec aux res = function
		| 0 -> res
		| x -> aux (res + f x) (x-1)
	in aux 0 n
	
let somme' = sommef (function x -> x)
let somme2' = sommef (function x -> x*x)


let rec dicho f (a,b) e = 
	assert ((f a) *. (f b) <= 0.);
	if (b -. a < e) then (a,b) else
		let m = (a+.b)/.2. in let c = f m in
		if ((f a) *. c <= 0. ) 
			then dicho f (a,m) e
			else dicho f (m,b) e