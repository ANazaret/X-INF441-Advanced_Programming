
type 'a tree
val node: int -> 'a -> 'a tree
val link: 'a tree -> 'a tree -> 'a tree 
val size: 'a tree -> int
val rank: 'a tree -> int

val root_priority : 'a tree -> int 
val root_value : 'a tree -> 'a 