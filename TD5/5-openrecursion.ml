let open_fib self = function
    | 0 -> 0
    | 1 -> 1
    | n -> self#fib (n - 1) + self#fib (n - 2);;



let pretty_dbg = object (self)
	val mutable indent = 0
  method fib n = 
		let indent_s = String.make (indent*2) ' ' in	
  		print_string indent_s; 
  		Printf.printf  "Calling fib(%d)\n" n ;
  		indent <- indent + 1;
      let res = open_fib self n in 
  			print_string indent_s; 
    		Printf.printf  "  Returning %d\n" res ;
    		indent <- indent - 1;
    		res		
end

let pretty_debug_fib = pretty_dbg#fib





type ocaml_type =
  | Int
  | Bool
  | Arrow of ocaml_type * ocaml_type
  | Product of ocaml_type list

type ocaml_expr =
  | Const_int of int
  | Const_bool of bool
  | Variable of string
  | If of ocaml_expr * ocaml_expr * ocaml_expr
  | Fun of string * ocaml_type * ocaml_expr
  | Tuple of ocaml_expr list
  | Apply of ocaml_expr * ocaml_expr
  | Let_in of ocaml_decl * ocaml_expr

and ocaml_decl =
  | Var_decl of string * ocaml_type * ocaml_expr
  | Rec_decl of (string * ocaml_type * ocaml_expr) list



class ocaml_map = object (self)
  method for_type (ty : ocaml_type) : ocaml_type = match ty with
		| Int -> Int
    | Bool -> Bool
    | Arrow(t1,t2) -> Arrow(self#for_type t1, self#for_type t2)
		| Product(l) -> Product(List.map (function x -> self#for_type x) l)
 

  method for_expr (ex : ocaml_expr) : ocaml_expr = match ex with
    | If(cond,the,els) -> If(self#for_expr cond,self#for_expr the, self#for_expr els)
    | Fun(str,typ,expr) -> Fun(str, self#for_type typ, self#for_expr expr)
    | Tuple(l) -> Tuple(List.map (function x -> self#for_expr x) l) 
    | Apply(x,y) -> Apply(self#for_expr x,self#for_expr y)
    | Let_in(decl,expr) -> Let_in(self#for_decl decl,self#for_expr expr)
		| x -> x

  method for_decl (dl : ocaml_decl) : ocaml_decl = match dl with 
	  | Var_decl(str,typ,expr) -> Var_decl(str,self#for_type typ,self#for_expr expr)
    | Rec_decl(l) -> Rec_decl(List.map (function (str,typ,expr) -> (str,self#for_type typ,self#for_expr expr)) l)

end