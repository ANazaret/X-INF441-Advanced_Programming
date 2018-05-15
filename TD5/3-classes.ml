class type shape = object
  method area : float
end

let larger : #shape -> #shape -> #shape =
  fun sh1 sh2 -> if sh1#area >= sh2#area then sh1 else sh2 
	
	
class rectangle (width, height : float * float) (x, y : float * float) = object (self)
  val mutable x = x
  val mutable y = y

  (* a reference point, e.g., the bottom left corner *)
  method key = (x, y)

  method area = width *. height

  (* return true if (px, py) is part of the rectangle *)
  method contains (px, py) =
    x <= px && px <= x +. width &&
    y <= py && py <= y +. height
end

let rect = new rectangle (10.,4.) (0.,0.)


class square (side : float) (x, y : float * float) = object
   inherit rectangle (side,side) (x,y)	
end

class circle radius (x,y) = object (self)
  (* a reference point, e.g., the bottom left corner *)
  method key = (x, y)

  method area = radius *. radius *. pi

  (* return true if (px, py) is part of the circle *)
  method contains (px, py) =
    (x-.px) *. (x-.px) +. (y-.py)*.(y-.py) <= radius*.radius
end


class box2 (w1, h1) (w2, h2) (x, y) = object (self)
  val left = new rectangle (w1,h1) (x,y)
	val right = new rectangle (w2,h2) (x+.w1,y)
	
	method key = (x, y)

  method area = left#area +. right#area

  (* return true if (px, py) is part of the circle *)
  method contains ((px, py) as c) =
    left#contains c || right#contains c
end




