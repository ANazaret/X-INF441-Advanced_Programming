open BinomialTree

val empty : 'a list
val add_tree : 'a tree list -> 'a tree -> 'a tree list 
val insert : 'a tree list -> int -> 'a -> 'a tree list 
val find_min : 'a tree list -> 'a 