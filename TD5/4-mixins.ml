class virtual describable name = object (self)
  val virtual mutable x : float
  val virtual mutable y : float
  method virtual area : float

  method describe =
    Printf.sprintf "Shape %S at (%f, %f), area %f"
      name x y self#area
end


class virtual movable = object (self)
  val virtual mutable x : float
  val virtual mutable y : float

  method move_x tx = x <- x +. tx
	method move_y ty = y <- y +. ty 
end

class virtual randomized = object(self)
	val virtual mutable x : float
  val virtual mutable y : float

	initializer x <- x +. Random.float 2. -. 1.; y <- y +. Random.float 2. -. 1.	
end	


class rectangle (width, height : float * float) (xx, yy : float * float) = object (self)
	inherit describable "rectangle"
	inherit movable
	
  val mutable x = xx
  val mutable y = yy

  (* a reference point, e.g., the bottom left corner *)
  method key = (x, y)

  method area = width *. height

  (* return true if (px, py) is part of the rectangle *)
  method contains (px, py) =
    x <= px && px <= x +. width &&
    y <= py && py <= y +. height

class fuzzed_rectangle (width, height) (xx, yy) = object (self)
	
	inherit randomized	
	inherit rectangle (width, height) (xx, yy) 
end
end

class fuzzed_rectangle (width, height) (xx, yy) = object (self)	
	inherit randomized	
	inherit rectangle (width, height) (xx, yy) 
end

let r = new fuzzed_rectangle (2.,5.) (0.,0.)