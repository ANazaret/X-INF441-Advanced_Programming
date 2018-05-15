let area o = o#width * o#height

let rectangle w h = 
	object
		method width = w
		method height = h
	end

let square s = 
	object
		method width = s
		method height = s
	end
	
let triangle b h =
	object
		method width = b
		method height = h
	end
	
	
let growable x y =
	object
		val mutable w = x
		val mutable h = y
		method width = w
		method height = h
		method grow_width i = w <- w+i
		method grow_height i = h <- h+i
	end



	
type shape = < area : float >
let pi = 4. *. atan 1.

let triangle b h : shape = 
	object
		val base = b
		val height = h
		method area = base *. height
	end
	
let square s : shape = 
	object (self)
		val side = s
		method area = side *. side
	end
	
let circle r : shape = 
	object (self)
		val radius = r
		method area = radius *. radius *. pi
	end
		
		
let t = triangle 2. 5.
let s = square 5.
let c = circle 4.

let l = [t;s;c]

let somme = List.fold_left (function res -> function (x:shape) -> res +. x#area ) 0.

