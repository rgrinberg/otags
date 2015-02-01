(* This test program demonstrates that Camlp4QuotationCommon.Make installs 
 * quotations per side-effect in the Quotation submodule of its Syntax 
 * argument. For creating a new syntax one therefore has to use fresh
 * Quotation submodules.
*)

module PC = Camlp4.PreCast


(* The simple functors for fresh grammars, which I use here, 
 * reuse certain modules from PreCast, therefore I need additional
 * equations in Camlp4Syntax to state the equality with those 
 * PreCast modules.
 *)
module type Camlp4Syntax = Camlp4.Sig.Camlp4Syntax 
  with module Loc = PC.Loc
  and module Ast = PC.Ast


(* Functor for building a fresh grammar that reuses the quotation module
 * from PreCast.
 *)
module FreshGrammar(Unit : sig end) 
  : Camlp4Syntax with module Token = PC.Gram.Token
                 and module Gram = PC.Gram
= Camlp4.OCamlInitSyntax.Make(PC.Ast)(PC.Gram)(PC.Quotation)


(* Functor for building a fresh grammar that contains a fresh 
 * quotation module.
 *)
module QuotFreshGrammar(Unit : sig end) 
  : Camlp4Syntax with module Token = PC.Gram.Token
                 and module Gram = PC.Gram
= Camlp4.OCamlInitSyntax.Make(PC.Ast)(PC.Gram)
		       (Camlp4.Struct.Quotation.Make(PC.Ast))


(* Functor to add quotations to a syntax *)
module Add_quotation
  (HostSyntax : Camlp4Syntax)
  (MakeQuotationSyntax : functor(EmptySyntax : Camlp4Syntax) -> Camlp4Syntax) 
  : Camlp4Syntax
  =
struct
  module XXX = struct
    module New_gram = PC.MakeGram(PC.Lexer)
    module EmptySyntax = 
      Camlp4.OCamlInitSyntax.Make(PC.Ast)(New_gram)(HostSyntax.Quotation)
    module QuotationSyntax = MakeQuotationSyntax(EmptySyntax)
    module X = 
      Camlp4QuotationCommon.Make(QuotationSyntax)(HostSyntax.AntiquotSyntax)
  end
  include HostSyntax
end

(* Module type for printout information *)
module type Pos = sig
    val pos : string
end

(* Functor to check a syntax for the presence of the str_item
 * quotation expander and print the result to stdout.
 *)
module Check_quot(P : Pos)(S : Camlp4Syntax) = struct
  let _ = 
    try
      S.Quotation.find "str_item" S.Quotation.DynAst.str_item_tag;
      Printf.printf "%s: quotation found\n%!" P.pos
    with
      | Not_found ->
	Printf.printf "%s: NO QUOTATION\n%!" P.pos
end



module A = FreshGrammar(struct end)
(* A is fresh and contains no quotations *)

module B = Camlp4OCamlRevisedParser.Make(A)
(* B is fresh revised syntax and contains no quotations *)


module X_A_1 = Check_quot(struct let pos = "A1" end)(A)
module X_B_1 = Check_quot(struct let pos = "B1" end)(B)


module C = Add_quotation(B)(Camlp4OCamlRevisedParser.Make)
(* Now A,B,C contain quotations *)

module X_A_2 = Check_quot(struct let pos = "A2" end)(A)
module X_B_2 = Check_quot(struct let pos = "B2" end)(B)
module X_C_2 = Check_quot(struct let pos = "C2" end)(C)


module D = FreshGrammar(struct end)
(* D is an empty grammar, but it contains quotations, because it
 * shares the quotation module with A,B,C *)

module X_A_3 = Check_quot(struct let pos = "A3" end)(A)
module X_B_3 = Check_quot(struct let pos = "B3" end)(B)
module X_C_3 = Check_quot(struct let pos = "C3" end)(C)
module X_D_3 = Check_quot(struct let pos = "D3" end)(D)


module E = Camlp4OCamlRevisedParser.Make(D)
(* E is a new revised syntax that contains already quotations *)

module X_A_4 = Check_quot(struct let pos = "A4" end)(A)
module X_B_4 = Check_quot(struct let pos = "B4" end)(B)
module X_C_4 = Check_quot(struct let pos = "C4" end)(C)
module X_D_4 = Check_quot(struct let pos = "D4" end)(D)
module X_E_4 = Check_quot(struct let pos = "E4" end)(E)


module F = QuotFreshGrammar(struct end)
module G = Camlp4OCamlRevisedParser.Make(F)
(* G is a fresh revised syntax with a fresh quotation module.
 * It contains no quotations.
 *)

module X_G_5 = Check_quot(struct let pos = "G5" end)(G)


module H = Add_quotation(G)(Camlp4OCamlRevisedParser.Make)
(* Now F,G,H contain quotations *)

module X_G_6 = Check_quot(struct let pos = "G6" end)(G)
module X_H_6 = Check_quot(struct let pos = "H6" end)(H)


module I = QuotFreshGrammar(struct end)
module J = Camlp4OCamlRevisedParser.Make(I)
(* J is another fresh revised syntax with a fresh quotation module.
 * It contains no quotations.
 *)

module X_G_7 = Check_quot(struct let pos = "G7" end)(G)
module X_H_7 = Check_quot(struct let pos = "H7" end)(H)
module X_J_7 = Check_quot(struct let pos = "J7" end)(J)


(*** Local Variables: ***)
(*** compile-command: "ocamlc.opt -I +camlp4 -I +camlp4/Camlp4Parsers dynlink.cma camlp4fulllib.cma test_qot.ml" ***)
(*** End: ***)
