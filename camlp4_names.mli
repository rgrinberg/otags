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
 * $Id: camlp4_names.mli,v 1.6 2012/05/22 07:38:36 tews Exp $
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
 * and for succint command lines dependencies are computed by camlp4,
 * such that "pa_op.cmo" translates to a whole bunch of modules to be 
 * loaded. All this is done in CamlprBin.rewrite_and_load. Unfortunately
 * one cannot reuse this code.
 * 
 * This module contains contains a copy of the relevant code from
 * CamlprBin.rewrite_and_load to achieve compatibility with camlp4.
 *)



(* Translates a parser name in the canonical camlp4 name with dependencies.
 * Knows and translates aliases like "of" or "rf". 
 * Raises Not_found for an unrecognized argument.
 *)
(* val parser_name_and_dependency : string -> string list *)

(** Translate a parser name into its canonical name and add its
    dependencies. Return the list of those parsers that are not
    already present in the first argument. Raise [Not_found] if the
    new parser is not one of the standard camlp4 parsers.
*)
val normalize_parser : string list -> string -> string list


(** Convert a list with canonical parser names into a string with
    shorter names.
*)
val short_string_of_parser_list : string list -> string

(* Takes a list of parser extensions and determines the camlp4
 * variant to use and the remaining parser extensions that need 
 * to be loaded at runtime.
 * The typical example is that the default parser list 
 * results in "camlp4o" with no remaining parsers.
 *)
val camlp4_variant : string list -> string * string list
