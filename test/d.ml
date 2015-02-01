
(* $Id: d.ml,v 1.1.1.1 2010/08/05 14:45:45 tews Exp $ *)

let f1 x = 12
(*
type a =
  | A of int  | B
*)
type a =
  | A of int
  | B  | C

type b = {g:int;           rr:int}

let f2 = function
  | A x -> x
  | y -> 10

;;
let f1 = 12 in f1
;;

module type END = sig 
  type end_t 
  val end_val : end_t
end

let rec f2 = function
  | A x -> x
  | y -> 10

let f3, f4 = 0, 1

let f5 as f6 = fun x -> x

let f7 : int -> int = fun x -> x

module Modu =
  struct let mof = 3 end

exception No_value

let value = function
  | None -> raise No_value
  | Some x -> x

class int_value init = object (self)

  val mutable r = init;

  method get = value r;

  method set n = r <- Some n;

  method add n = match r with
    | None -> self#set n
    | Some m -> r <- Some (n + m);

end

let f8 : int -> int = fun x -> x

(* Bug pointed by Hendrik Tews *)
(* Status: not fixed *)

type a_type = 
  | A1 of b_type
  | A2 of a_type

and b_type = 
  | B1 of b_type
  | B2 of a_type


class ['a] c (a : 'a ) = object 
  method b = a 
end

and ['a] d (a : 'a ) = object 
  method dm = a 
end

class b_class = [int * int] c

class x = 
  (fun o ->  object method x = o end) 5

class type x_class = object
  val mutable y : int
end
;;

let ( <:> ) x y = x + y;;


(*** Local Variables: ***)
(*** compile-command: "make -C .. test" ***)
(*** End: ***)
