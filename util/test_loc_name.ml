(* Attempt to fix the global Loc.name problem by turning the
 * Loc module into a functor. 
 * 
 * For the Make_Loc functor I reuse the original Loc source code
 * via INCLUDE of pa_macro, however, this forces me to use the 
 * revised syntax. Additionally, the Loc source code uses the debug 
 * syntax extension, which requires Camlp4DebugParser.cmo.
 * 
 * To check Loc.name I parse some code with a quotation print the 
 * expansion. The code would be invalid, if really compiled with a 
 * changed Loc.name.
 *)

module PC = Camlp4.PreCast;

module type Camlp4Syntax = Camlp4.Sig.Camlp4Syntax;

(* the following abbreviation is needed, because Loc.ml names 
 * ErrorHandler directly, while in the distribution it is packed 
 * in the Camlp4 module.
 *)
module ErrorHandler = Camlp4.ErrorHandler;

module Make_Loc(Unit : sig end) : Camlp4.Sig.Loc =
struct
  INCLUDE "/usr/local/src/ocaml-3.12.1/camlp4/Camlp4/Struct/Loc.ml";
end;


(* functor to build a fresh, empty syntax
 * this version implicitely reuses PreCast.Loc and therefore
 * Loc.name is shared for all grammars build with this functor.
 *)
module FreshGrammar(Unit : sig end) : Camlp4Syntax = 
  Camlp4.OCamlInitSyntax.Make(PC.Ast)
		       (Camlp4.Struct.Grammar.Static.Make(PC.Lexer))
		       (Camlp4.Struct.Quotation.Make(PC.Ast));


(* functor to build a fresh, empty syntax
 * This version builds a fresh Loc module, everytime it is used.
 * Loc.name is not shared any longer.
 *)
module LocFreshGrammar(Unit : sig end) : Camlp4Syntax = 
struct
  module XXX = struct
    module Loc = Make_Loc(Unit);
    module Ast = Camlp4.Struct.Camlp4Ast.Make Loc;
    module Token = Camlp4.Struct.Token.Make Loc;
    module Lexer = Camlp4.Struct.Lexer.Make Token;
    module Gram = Camlp4.Struct.Grammar.Static.Make Lexer;
    module Quotation = Camlp4.Struct.Quotation.Make Ast;
    module Fresh_Syntax = 
      Camlp4.OCamlInitSyntax.Make Ast Gram Quotation;
  end;
  include XXX.Fresh_Syntax;
end;


(* functor to add revised quotations to some syntax *)
module Add_quotations(HostSyntax : Camlp4Syntax) : Camlp4Syntax =
struct
  module XXX = struct
    module Lexer = Camlp4.Struct.Lexer.Make(HostSyntax.Token);
    module New_gram = Camlp4.Struct.Grammar.Static.Make(Lexer);
    module EmptySyntax = 
      Camlp4.OCamlInitSyntax.Make(HostSyntax.Ast)(New_gram)
	(HostSyntax.Quotation);
    module QuotationSyntax = Camlp4OCamlRevisedParser.Make(EmptySyntax);
    module X = 
      Camlp4QuotationCommon.Make(QuotationSyntax)(HostSyntax.AntiquotSyntax);
  end;
  include HostSyntax;
end;


(* give Names to all modules, because otherwise typing may fail *)
module Unit = struct end;

(* Functor to build a fresh ocaml syntax with revised quotations.
 * The argument FreshGram is used to build an fresh, empty grammar,
 * it can either be FreshGrammar or LocFreshGrammar.
 *)
module Camlp4or(FreshGram : functor(Unit : sig end) -> Camlp4Syntax) 
  : Camlp4Syntax =
  (* this version is fine *)
  Add_quotations
    (Camlp4.Printers.OCaml.Make
       (Camlp4OCamlParser.Make
  	  (Camlp4OCamlRevisedParser.Make(FreshGram Unit))));


(* functor to change Loc.name of some syntax *)
module Change_loc(S : Camlp4Syntax) : Camlp4Syntax =
struct
  do { S.Loc.name.val := "_xxx" };
  include S;
end;


(* module type for debugging position info *)
module type Pos = sig
    value pos : string;
end;


(* functor that checks Loc.name by expanding and pretty-printing some
 * quotation code.
 *)
module Check_loc(P : Pos)(S : Camlp4Syntax) = struct
  value loc_input = "let f _loc = <:expr< 5 >>";
  value ast = 
    try
      S.parse_implem (S.Loc.mk "-") (Stream.of_string loc_input)
    with [
      S.Loc.Exc_located(_, ex) ->
	do {
	  Printf.eprintf "XXX: %s\n" (Printexc.to_string ex);
	  raise ex
	}
    ];
  do { 
    print_string P.pos; 
    print_string ": ";
    S.print_implem ast      
  };
end;


module A = Camlp4or(FreshGrammar);
(* A is the first syntax, it has Loc.name = "_loc" *)

module X_A_1 = Check_loc(struct value pos = "A1"; end)(A);

module B = Change_loc(A);
(* now A and B have Loc.name = "_xxx" *)

module X_A_2 = Check_loc(struct value pos = "A2"; end)(A);
module X_B_2 = Check_loc(struct value pos = "B2"; end)(B);

module C = Camlp4or(FreshGrammar);
(* next fresh syntax, because of the Loc.name sharing problem 
 * also C has Loc.name = "_xxx" *)

module X_A_3 = Check_loc(struct value pos = "A3"; end)(A);
module X_B_3 = Check_loc(struct value pos = "B3"; end)(B);
module X_C_3 = Check_loc(struct value pos = "C3"; end)(C);


module D = Camlp4or(LocFreshGrammar);
(* The next grammar with a fresh Loc module inside. 
 * D has Loc.name = "_loc" *)

module X_D_4 = Check_loc(struct value pos = "D4"; end)(D);

module E = Change_loc(D);
(* Now D and E have Loc.name = "_xxx" *)

module X_D_5 = Check_loc(struct value pos = "D5"; end)(D);
module X_E_5 = Check_loc(struct value pos = "E5"; end)(E);

module F = Camlp4or(LocFreshGrammar);
(* The next fresh syntax F has Loc.name "_loc" again *)

module X_D_6 = Check_loc(struct value pos = "D6"; end)(D);
module X_E_6 = Check_loc(struct value pos = "E6"; end)(E);
module X_F_6 = Check_loc(struct value pos = "F6"; end)(F);



(*** Local Variables: ***)
(*** compile-command: "ocamlc.opt -g -pp 'camlp4r -parser macro Camlp4DebugParser.cmo' -I +camlp4 -I +camlp4/Camlp4Parsers dynlink.cma camlp4/camlp4fulllib.cma test_loc_name.ml" ***)
(*** End: ***)
