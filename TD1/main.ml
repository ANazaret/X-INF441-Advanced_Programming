let pmap f g = fun (x,y) -> (f x, g y);;

let idc x = (fun y -> y + x ), (fun y -> y - x);;

let comp f g x = f (g x);;
Type :
val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>


let (ic, dc) = idc 10 in comp ic dc
Renvoie l'identite

