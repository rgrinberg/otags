(* Exercise in exception handling: Catch a Quotation error exception *)

module Quotation = Camlp4.Struct.Quotation.Make(Camlp4.PreCast.Ast)

module Syntax = 
  Camlp4.OCamlInitSyntax.Make(Camlp4.PreCast.Ast)(Camlp4.PreCast.Gram)(Quotation)

module S1 = Camlp4OCamlRevisedParser.Make(Syntax)

module S2 = Camlp4OCamlParser.Make(Syntax)

let start_loc = Syntax.Loc.mk "-"

let input_stream = Stream.of_string 
  "leöt empty_sig_ast = Sig_ast(<:sig_item@no_loc< >>)"

let _ = 
  try
    Syntax.parse_implem start_loc input_stream
  with
    | ex1 ->
      prerr_endline (Printexc.to_string ex1);
      (match ex1 with
	| Syntax.Loc.Exc_located(_loc, ex2) ->
	  prerr_endline(Printexc.to_string ex2);
	  (match ex2 with
	    | Syntax.Quotation.Error.E error -> 
	      prerr_endline (Syntax.Quotation.Error.to_string error)
	    | Camlp4.PreCast.Lexer.Error.E error -> 
	      prerr_endline (Camlp4.PreCast.Lexer.Error.to_string error)
	    | _ ->
	      prerr_endline "ex2 match failed"
	  )
	| _ -> 
	  prerr_endline "ex1 match failed"
      );
      exit 1



(*** Local Variables: ***)
(*** compile-command: "ocamlc -o catch_quot_error -g -I +camlp4 -I +camlp4/Camlp4Parsers dynlink.cma camlp4fulllib.cma catch_quot_error.ml" ***)
(*** End: ***)
