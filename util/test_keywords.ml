(* This sample program demonstrates that keywords are stored inside 
 * the Grammar module. Therefore, for a fresh syntax one needs a fresh
 * grammar.
*)

module PC = Camlp4.PreCast


module type Camlp4Syntax = Camlp4.Sig.Camlp4Syntax 
  with module Loc = PC.Loc
  and module Ast = PC.Ast


(* functor to build a fresh, empty syntax with reusing the grammar
 * module from Camlp4.PreCast
 *)
module FreshGrammar(Unit : sig end) : Camlp4Syntax 
= Camlp4.OCamlInitSyntax.Make(PC.Ast)(PC.Gram)
		       (Camlp4.Struct.Quotation.Make(PC.Ast))


(* functor to build a fresh, empty syntax with a fresh grammar *)
module FreshFreshGrammar(Unit : sig end) : Camlp4Syntax 
= Camlp4.OCamlInitSyntax.Make(PC.Ast)
		       (Camlp4.Struct.Grammar.Static.Make(PC.Lexer))
		       (Camlp4.Struct.Quotation.Make(PC.Ast))


(* module type for debugging position info *)
module type Pos = sig
    val pos : string
end


(* functor that checks whether "val" is a keyword in syntax S *)
module Check_keyword_val(P : Pos)(S : Camlp4Syntax) = struct
  let token_filter = S.Token.Filter.filter (S.Gram.get_filter())
  let val_token_stream = [< '( Camlp4.Sig.SYMBOL("val"), S.Loc.ghost) >]
  let filtered_stream = token_filter val_token_stream
  let _ = match filtered_stream with parser
    | [< '(Camlp4.Sig.KEYWORD x, _ ) >] ->
      Printf.printf "%s: KEYWORD %s\n%!" P.pos x
    | [< '(Camlp4.Sig.SYMBOL x, _ ) >] ->
      Printf.printf "%s: SYMBOL %s\n%!" P.pos x
end


module A = FreshGrammar(struct end)
(* now A is empty *)

module X_A_1 = Check_keyword_val(struct let pos = "A1" end)(A)

module B = Camlp4OCamlRevisedParser.Make(A)
(* now A, B parse the revised syntax, val is no keyword *)

module X_A_2 = Check_keyword_val(struct let pos = "A2" end)(A)
module X_B_2 = Check_keyword_val(struct let pos = "B2" end)(B)

module C = Camlp4OCamlParser.Make(B)
(* now A,B,C parse the original syntax, val is a keyword *)

module X_A_3 = Check_keyword_val(struct let pos = "A3" end)(A)
module X_B_3 = Check_keyword_val(struct let pos = "B3" end)(B)
module X_C_3 = Check_keyword_val(struct let pos = "C3" end)(C)

module D = FreshGrammar(struct end)
(* D is fresh but shares the grammar module with A,B,C and 
 * therefore also the hashtable with keywords. Therefore
 * val is a keyword in D
 *)

module X_A_4 = Check_keyword_val(struct let pos = "A4" end)(A)
module X_B_4 = Check_keyword_val(struct let pos = "B4" end)(B)
module X_C_4 = Check_keyword_val(struct let pos = "C4" end)(C)
module X_D_4 = Check_keyword_val(struct let pos = "D4" end)(D)

module E = Camlp4OCamlRevisedParser.Make(D)
(* D,E parse the revised syntax, but "val" is filtered as
 * a keyword!
 *)

module X_A_5 = Check_keyword_val(struct let pos = "A5" end)(A)
module X_B_5 = Check_keyword_val(struct let pos = "B5" end)(B)
module X_C_5 = Check_keyword_val(struct let pos = "C5" end)(C)
module X_D_5 = Check_keyword_val(struct let pos = "D5" end)(D)
module X_E_5 = Check_keyword_val(struct let pos = "E5" end)(E)

module F = FreshFreshGrammar(struct end)
(* F is fresh with a fresh grammar module, val is no keyword in F *)

module X_A_6 = Check_keyword_val(struct let pos = "A6" end)(A)
module X_B_6 = Check_keyword_val(struct let pos = "B6" end)(B)
module X_C_6 = Check_keyword_val(struct let pos = "C6" end)(C)
module X_D_6 = Check_keyword_val(struct let pos = "D6" end)(D)
module X_E_6 = Check_keyword_val(struct let pos = "E6" end)(E)
module X_F_6 = Check_keyword_val(struct let pos = "F6" end)(F)

module G = Camlp4OCamlRevisedParser.Make(F)
(* now F,G really parse the revised syntax without val being a keyword *)

module X_A_7 = Check_keyword_val(struct let pos = "A7" end)(A)
module X_B_7 = Check_keyword_val(struct let pos = "B7" end)(B)
module X_C_7 = Check_keyword_val(struct let pos = "C7" end)(C)
module X_D_7 = Check_keyword_val(struct let pos = "D7" end)(D)
module X_E_7 = Check_keyword_val(struct let pos = "E7" end)(E)
module X_F_7 = Check_keyword_val(struct let pos = "F7" end)(F)
module X_G_7 = Check_keyword_val(struct let pos = "G7" end)(G)

(*** Local Variables: ***)
(*** compile-command: "ocamlc.opt -g -pp camlp4o -I +camlp4 -I +camlp4/Camlp4Parsers dynlink.cma camlp4fulllib.cma test_keywords.ml" ***)
(*** End: ***)
