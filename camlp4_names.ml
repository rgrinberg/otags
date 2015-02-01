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
 * $Id: camlp4_names.ml,v 1.12 2012/05/22 07:38:36 tews Exp $
 *
 * names and aliases of camlp4 modules and executables
 *
 *)

(* The new camlp4 prefers long names for parsing extensions, for instance
 * "Camlp4OCamlRevisedParser". To make succint command lines possible
 * and to provide some backwards compatibility there exist a number
 * of alias names. Camlp4OCamlRevisedParser can for instance be referred
 * under all of "pa_r.cmo", "r", "ocamlr" "ocamlrevised",
 * "camlp4ocamlrevisedparser.com". Further, there are more dependencies
 * between the syntax extensions, for instance the original syntax is
 * now an extension of the revised one. Again for backwards compatibility
 * and for succint command lines, dependencies are computed by camlp4,
 * such that "pa_op.cmo" translates to a whole bunch of modules to be
 * loaded. All this is done in CamlprBin.rewrite_and_load. Unfortunately
 * one cannot reuse this code.
 *
 * This module contains contains a copy of the relevant code from
 * CamlprBin.rewrite_and_load to achieve compatibility with camlp4.
 *)


let pa_r  = "Camlp4OCamlRevisedParser"
let pa_rr = "Camlp4OCamlReloadedParser"
let pa_o  = "Camlp4OCamlParser"
let pa_rp = "Camlp4OCamlRevisedParserParser"
let pa_op = "Camlp4OCamlParserParser"
let pa_g  = "Camlp4GrammarParser"
let pa_m  = "Camlp4MacroParser"
let pa_qb = "Camlp4QuotationCommon"
let pa_q  = "Camlp4QuotationExpander"
let pa_rq = "Camlp4OCamlRevisedQuotationExpander"
let pa_oq = "Camlp4OCamlOriginalQuotationExpander"
let pa_l  = "Camlp4ListComprehension"


(* modules not in Camlp4Bin *)
let pa_d  = "Camlp4DebugParser"


(* Translates a parser name in the canonical camlp4 name with dependencies.
 * Knows and translates aliases like "of" or "rf".
 * Raises Not_found for an unrecognized argument.
 *)
let parser_name_and_dependency parser_name =
  match String.lowercase parser_name with
    | "pa_r.cmo"
    | "r"
    | "ocamlr"
    | "ocamlrevised"
    | "camlp4ocamlrevisedparser.cmo"
      -> [pa_r]

    | "rr"
    | "reloaded"
    | "ocamlreloaded"
    | "camlp4ocamlreloadedparser.cmo"
      -> [pa_rr]

    | "pa_o.cmo"
    | "o"
    | "ocaml"
    | "camlp4ocamlparser.cmo"
      -> [pa_r; pa_o]

    | "pa_rp.cmo"
    | "rp"
    | "rparser"
    | "camlp4ocamlrevisedparserparser.cmo"
	(* Camlp4Bin 3.12.0 and before loads pa_o, see #5134 *)
      -> [pa_r; pa_rp]

    | "pa_op.cmo"
    | "op"
    | "parser"
    | "camlp4ocamlparserparser.cmo"
      -> [pa_r; pa_o; pa_rp; pa_op]

    | "pa_extend.cmo"
    | "pa_extend_m.cmo"
    | "g"
    | "grammar"
    | "camlp4grammarparser.cmo"
      -> [pa_g]

    | "pa_macro.cmo"
    | "m"
    | "macro"
    | "camlp4macroparser.cmo"
      -> [pa_m]

    | "q"
    | "camlp4quotationexpander.cmo"
      -> [pa_qb; pa_q]

    | "q_mlast.cmo"
    | "rq"
    | "camlp4ocamlrevisedquotationexpander.cmo"
      -> [pa_qb; pa_rq]

    | "oq"
    | "camlp4ocamloriginalquotationexpander.cmo"
      -> [pa_r; pa_o; pa_qb; pa_oq]

    | "rf"
      -> [pa_r; pa_rp; pa_qb; pa_q; pa_g; pa_l; pa_m]

    | "of"
      -> [pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_q; pa_g; pa_l; pa_m]

    | "comp"
    | "camlp4listcomprehension.cmo"
      -> [pa_l]

    (* Cases not or not explicitely treated in Camlp4Bin *)

    | "debug"
    | "camlp4debugparser"
    | "camlp4debugparser.cmo"
      -> [pa_d]

    | _
      -> raise Not_found


let normalize_parser parser_list new_parser =
  List.filter 
    (fun pa -> not (List.mem pa parser_list))
    (parser_name_and_dependency new_parser)


let short_names_assoc = [
  (pa_r,  "r");
  (pa_rr, "rr");
  (pa_o,  "o");
  (pa_rp, "rp");
  (pa_op, "op");
  (pa_g,  "g");
  (pa_m,  "m");
  (pa_qb, "qc");
  (pa_q,  "q");
  (pa_rq, "rq");
  (pa_oq, "oq");
  (pa_l,  "comp");
  (pa_d,  "debug");
]


let short_string_of_parser_list parser_list =
  let l = 
    List.map
      (fun long_name ->
	try List.assoc long_name short_names_assoc
	with Not_found -> long_name)
      parser_list
  in
  "[" ^ (String.concat "; " l) ^ "]"

let rec split_list n = function
  | [] -> ([], [])
  | x::l as xl ->
    if n = 0
    then ([], xl)
    else
      let (head, tail) = split_list (n - 1) l in
      (x :: head, tail)


let empty_camlp4 = "camlp4"


(* Every veriant in this assoc list must be checked in the configure
 * script. When you add a variant here, then also add it to the
 * configure script!
 *)
let camlp4_variant_assoc = [
  ([pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_q; pa_g; pa_l; pa_m],
   "camlp4of");

  (* the next line gives the order in which camlp4of is actually linked. *)
  ([pa_r; pa_qb; pa_q; pa_o; pa_rp; pa_op; pa_g; pa_m; pa_l; ],
   "camlp4of");

  ([pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_oq; pa_g; pa_m; pa_l; ],
   "camlp4oof");

  ([pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_rq; pa_g; pa_m; pa_l; ],
   "camlp4orf");

  ([pa_r; pa_qb; pa_q; pa_rp; pa_g; pa_m; pa_l; ],
   "camlp4rf");

  ([pa_r; pa_rp; pa_qb; pa_q; pa_g; pa_l; pa_m],
   "camlp4rf");

  ([pa_r; pa_o; pa_rp; pa_op; ],
   "camlp4o");

  ([pa_r; pa_rp; ],
   "camlp4r");
]



let rec check_camlp4 parser_list = function
  | [] -> (empty_camlp4, parser_list)
  | (parsers, camlp4) :: ovariants ->
    let (head, tail) = split_list (List.length parsers) parser_list in
    if head = parsers
    then (camlp4, tail)
    else check_camlp4 parser_list ovariants


let camlp4_variant parser_list =
  check_camlp4 parser_list camlp4_variant_assoc
