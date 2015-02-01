
let a0 = 5

let f0 x y z = 6

let (a1, a2, a3) = (1,2,3)

let [a4; a5] = [1; 2]

let [a6] | [a6;_] = [1]

let () = ()

let _ = 7

let (a7 : int) = 1

exception A8

exception A9 = A8

exception A10 of int * string

external a11 : int -> int = "a13"

module A12 = Int32

module A13 = functor (X : Map.S) -> Int32

module A14 = struct
  let a15 x = x + 10
end

module A16 = functor(X : Map.S) ->
  struct
    let a16 = 16
  end

module A17(X : Map.S) = struct
  let a18 = 18
end

module type A18A = sig
  type tx
  val compare : tx -> tx -> int
end

module A19 =
  (struct
    type tx = int
    let compare x y = 1
   end
     : A18A)

module A20 : A18A = struct
  type tx = int
  let compare x y = 1
end
    
module type A21 = Map.S

module type A22 = sig
  exception A22a of int * int
end

module type A23 = 
  functor ( X : sig exception A24 of int end ) ->
sig
  exception A25 of int * int
end

module type A26 = 
sig
  type tx
  exception A22a of int * int
end
  with type tx = Int32.t

type a27 = int

type 'a a28 = int

type ('a, 'b, 'c) a29 = int

type + ' abc a30 = int

type - ' abc a31 = int

type a32 = 
  | A33
  | A34 of int
  | A35 of int * float

type a36 = {
  a37 : int;
  mutable a38 : int * int
}

module type A39 = sig
  class type a40 = object end
  class ['a, 'b ] a41 : int -> a40
  class a42 : object
    method a43 : int
  end
end

class type virtual ['a, 'b] a44 = 
object ('self)
  method a45 : unit
  method private a46 : unit
  method private virtual a47 : unit
  method virtual a48 : unit
  val a49 : int
  val mutable a50 : int
  (* 
   * val virtual a51 : int
   * val mutable virtual a52 : int
   *)
end

class type a53 = object end

class virtual ['a, 'b] a54 a b = 
  let xa = a 
  in
object 
  inherit object method a55 = 0 end as anon
val a56 : 'a = xa
val mutable a57 = 8
(* 
 * val virtual a58 : int
 * val virtual mutable a59 : int
 *)
method virtual a60 : int
method virtual private a61 : int
method a62 = (b : 'b)
method private a63 = 8
constraint int = int
initializer ignore(anon#a55)
end

let ( <:> ) x y = x + y;;

let a64 = "\\" (* /\ *)

let a65 = '/'				(* \/ *)

class type a66 = object
  inherit object method a67 : int end
end

class a67 = 
  (object
    method a68 = 5
   end 
  : object
    method a68 : int
  end)

let _a69 = 5

type _a70 = int

type a80 = A81

type a82 = a80 = A81

let ( ! ) a b = a + b

external ( ! ) : 'a ref -> 'a = "%field0"

module rec A82 : A18A = struct
  type tx = int
  let compare = compare
end
and A83 : sig 
  val a84 : int 
end = struct
  let a84 = 5
end

module type A85 = sig end

module A86 = (val (module struct end : A85) : A85)


type 'a a87_term =
  | Int : int -> int a87_term
  | Add : (int -> int -> int) a87_term
  | App : ('b -> 'a) a87_term * 'b a87_term -> 'a a87_term

let rec a88_eval : type a. a a87_term -> a = function
  | Int n    -> n
  | Add      -> (fun x y -> x+y)
  | App(f,x) -> (a88_eval f) (a88_eval x)


(*** Local Variables: ***)
(*** compile-command: "make -C .. testtrue" ***)
(*** End: ***)
