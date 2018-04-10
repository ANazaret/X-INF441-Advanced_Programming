let rec fact  = function 
	| 0 -> 1
	| n -> n* fact (n-1);;  

let rec fact_r r x = match x with 
| 0 -> r
| n -> fact_r (r*n) (n-1);;



let fact_term x = fact_r 1 x;;

let rec exp x = function
	| 0 -> 1
	| n -> x* (exp x (n-1) ) ;;

let rec exp_r r x = function 
	| 0 -> r
	| n -> exp_r (r*x) x (n-1);;

let rec fast_exp x = function
	| 0 -> 1
	| n when n mod 2=0 -> fast_exp (x*x) (n/2)
	| n -> fast_exp (x*x) (n/2) * x;;


let rec even = function 
	| 0 -> true
	| n when n>0 -> odd (n-1)
	| n -> even (-n)

and odd = function
	| 0 -> false
	| n when n>0 -> even (n-1)
	| n -> odd (-n)
;;


let rec fibo_r a b = function
	| 0 -> a
	| 1 -> b
	| n -> fibo_r b (a+b) (n-1)
;;

let fibo n = fibo_r 1 1 n;;

