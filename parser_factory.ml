(* Otags reloaded
 * 
 * Hendrik Tews Copyright (C) 2010 - 2012
 * 
 * This file is part of "Otags reloaded".
 * 
 * "Otags reloaded" is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * "Otags reloaded" is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the parent
 * directories for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with "Otags reloaded". If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 * $Id: parser_factory.ml,v 1.19 2012/05/21 09:29:29 tews Exp $
 * 
 * build new camlp4 parsers
 * 
 *)

open Monitor_line_directive
open Global
open Types

let camlp4_error msg =
  "Camlp4 parse error: " ^ msg


type parser_functions = { 
  mkloc : string -> loc_t;
  parse_implem : 
    ?directive_handler:(str_item_t -> str_item_t option) -> 
		                       loc_t -> char Stream.t -> str_item_t;
  parse_interf : 
    ?directive_handler:(sig_item_t -> sig_item_t option) -> 
		                       loc_t -> char Stream.t -> sig_item_t;
}

let current_parser = ref {
  mkloc = (fun _ -> assert false);
  parse_implem = (fun ?directive_handler:_dh _ -> assert false);
  parse_interf = (fun ?directive_handler:_dh _ -> assert false);
}


(* functor for creating a fresh grammar *)
module FreshGrammar(Unit : sig end) 
  : Camlp4.Sig.Camlp4Syntax with module Loc = Camlp4.PreCast.Loc
			    and module Ast = Camlp4.PreCast.Ast
= Camlp4.OCamlInitSyntax.Make
    (Camlp4.PreCast.Ast)
    (Camlp4.Struct.Grammar.Static.Make(Camlp4.PreCast.Lexer))
    (Camlp4.Struct.Quotation.Make(Camlp4.PreCast.Ast))


(* List of camlp4 standard parsers with their long names.
 * All these are treated internally in the following function.
 * 
 * Camlp4OCamlRevisedParser
 * Camlp4OCamlReloadedParser
 * Camlp4OCamlParser
 * Camlp4OCamlRevisedParserParser
 * Camlp4OCamlParserParser
 * Camlp4GrammarParser
 * Camlp4MacroParser
 * Camlp4QuotationCommon
 * Camlp4QuotationExpander
 * Camlp4OCamlRevisedQuotationExpander
 * Camlp4OCamlOriginalQuotationExpander
 * Camlp4ListComprehension
 * 
 * modules not treated Camlp4Bin:
 * 
 * Camlp4DebugParser
 *)


(* Build new parsing funktions corresponding to the syntax extensions
 * in the parser_list argument.
 * 
 * The syntax extensions must be applied with functor application to 
 * some Camlp4Syntax module. Therefore, this function makes a local 
 * Camlp4Syntax module, applies all parsing functions and extracts the 
 * fields that are necessary for us into a parser_functions record.
 *)
let build_parser parser_list =
  if !verbose then
    Printf.eprintf "Build new parser out of %s\n"
      (String.concat " " parser_list);

    (* Whether the lexer lexes quotations and antiquotations is controlled
     * by the global references in Camlp4_config (which is not very 
     * modular/reentrant). I beliefe lexing quotations and antiquotations
     * should be off by default and only enabled if some parsing extensions 
     * (notably some that installs quotations) needs them. Antiquotations
     * are turned on inside quotations, see 
     * Camlp4QuotationCommon.add_quotation.
     * 
     * I use the following heuristic here: Adding quotations enables 
     * quotations and macro parser enables antiquotations (otherwise 
     * Camlp4MacroParser.ml does not parse.
     *)
  let parse_quotations = ref false in
  let parse_antiquotations = ref false in

    (* This is the local module to which all grammar extension are 
     * applied to. I cannot pass this module into a different function, 
     * therefore, many utility functions, that I would like to
     * factor out, have to be defined inside build_parser.
     *)
  let module Otags_syntax = FreshGrammar(struct end) in

    (* This subfunction applies one parsing extension to Otags_syntax.
     * The correspondes between names of parsing extensions (ie. strings)
     * and the parsing extension functors is hardwired here. 
     * It would be much better if there were a hash table that maps 
     * names to functors, such that each parsing extension could register 
     * oneself there.
     *)
  let apply_parsing_extensions = function
    | "Camlp4OCamlRevisedParser" ->
      let module M = Camlp4OCamlRevisedParser.Make(Otags_syntax) in ()

    (* 
     * | "Camlp4OCamlReloadedParser" ->
     * 	let module M = Camlp4OCamlReloadedParser.Make(Otags_syntax) in ()
     *)

    | "Camlp4OCamlParser" ->
	let module M = Camlp4OCamlParser.Make(Otags_syntax) in ()

    | "Camlp4OCamlRevisedParserParser" ->
	let module M = Camlp4OCamlRevisedParserParser.Make(Otags_syntax) in ()

    | "Camlp4OCamlParserParser" ->
	let module M = Camlp4OCamlParserParser.Make(Otags_syntax) in ()

    | "Camlp4GrammarParser" ->
	let module M = Camlp4GrammarParser.Make(Otags_syntax) in ();
	parse_antiquotations := true

    | "Camlp4MacroParser" ->
	let module M = Camlp4MacroParser.Make(Otags_syntax) in ()

    | "Camlp4QuotationCommon" ->
      (* The Camlp4QuotationCommon module provides a Make functor, 
       * but this Make takes two arguments (so it does not fit the pattern 
       * here) and is not meant to be applied to any syntax when the module 
       * is loaded. Instead other quotation-building functors rely on 
       * this Make functor. Therefore we should not do anything here.
      *)
      ()

    | "Camlp4QuotationExpander" ->
      let module M = Camlp4QuotationExpander.Make(Otags_syntax) in ();
      parse_quotations := true

    | "Camlp4OCamlRevisedQuotationExpander" ->
      let module M =
	    Add_quotation.Make(Otags_syntax)(Camlp4OCamlRevisedParser.Make)
      in ();
      parse_quotations := true

    | "Camlp4OCamlOriginalQuotationExpander" ->
      let module OS = 
	    functor(S : Camlp4.Sig.Camlp4Syntax) ->
	      struct
		include 
		  Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(S))
	      end
      in
      let module M =
	    Add_quotation.Make(Otags_syntax)(OS)
      in ();
      parse_quotations := true

    | "Camlp4ListComprehension" ->
	let module M = Camlp4ListComprehension.Make(Otags_syntax) in ()

    | "Camlp4DebugParser" ->
      let module M = Camlp4DebugParser.Make(Otags_syntax) in ()

    | _ -> assert false
  in
  List.iter apply_parsing_extensions parser_list;

  let module M = Line_directive_monitor(Otags_syntax) in

    (* Exceptions catch and error reporting wrapper for a parsing function.
     * This wrapper has to stay in the scope of Otags_syntax because
     * it accesses exceptions and error formatting functions from it.
     *)
  let catch_some_exc f = fun ?directive_handler x y ->
    try
      f ?directive_handler x y 
    with
      | Otags_syntax.Loc.Exc_located(loc, Stream.Error msg) ->
	raise(Otags_parsing_error(loc, camlp4_error msg))
      | Otags_syntax.Loc.Exc_located(loc, Sys_error msg) ->
	raise(Otags_parsing_error(loc, camlp4_error msg))
      | Otags_syntax.Loc.Exc_located(loc, Otags_syntax.Quotation.Error.E err) ->
	raise(Otags_parsing_error(loc, 
		    camlp4_error (Otags_syntax.Quotation.Error.to_string err)))
      | Otags_syntax.Loc.Exc_located(loc, Camlp4.PreCast.Lexer.Error.E err) ->
	raise(Otags_parsing_error(loc,
		    camlp4_error(Camlp4.PreCast.Lexer.Error.to_string err)))
  in

    (* Wrapper function to set the global variables for lexing/parsing
     * quotations and antiquotations in Camlp4_config.
     *)
  let set_quotation_flags f = fun ?directive_handler x y ->
    Camlp4_config.quotations := !parse_quotations;
    Camlp4_config.antiquotations := !parse_antiquotations;
    f ?directive_handler x y
  in

    (* Extract the necessary fields from Otags_syntax and store
     * them in the result record
     *)
  { mkloc = Otags_syntax.Loc.mk;
    parse_implem = 
      catch_some_exc (set_quotation_flags Otags_syntax.parse_implem);
    parse_interf = 
      catch_some_exc (set_quotation_flags Otags_syntax.parse_interf);
  }




let current_parser_list = ref []

let update_syntax parser_list =
  assert(parser_list <> []);
  if !current_parser_list != parser_list 
  then begin
    (* let pa_start = Unix.gettimeofday() in *)
    current_parser := build_parser parser_list;
    (* 
     * let time = Unix.gettimeofday() -. pa_start in
     * Printf.printf "parser %s build time %.2f ms\n%!"
     *   (String.concat " " parser_list)
     *   (time *. 1000.0);
     *)
    current_parser_list := parser_list;
  end;
  !current_parser
