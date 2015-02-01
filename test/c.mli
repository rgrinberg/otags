
val c0 : int

exception C1

external c2 : int -> int = "c2"

module C3 : Map.S

module type C4 = sig
  type c5
end

type c6

type 'aaaa c7

type ('aaaa, 'bbbbbb) c8

val (@@) : 'a -> 'b -> 'c

val ( *@@ ) : 'a -> 'b -> 'c

module rec C9 : C4
and C11 : sig
  type c12
end

module type c13

(*** Local Variables: ***)
(*** compile-command: "make -C .. test" ***)
(*** End: ***)
