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
 * $Id: types.mli,v 1.10 2012/01/23 14:27:30 tews Exp $
 * 
 * global type definitions
 * 
 *)

type unit_type =
  | Signature
  | Structure

val string_of_unit_type : unit_type -> string


(* XXX Why/What about type loc_t = Caml4.Struct.Loc.t ??? *)
type loc_t = Camlp4.PreCast.Loc.t
type str_item_t = Camlp4.PreCast.Ast.str_item
type sig_item_t = Camlp4.PreCast.Ast.sig_item

type comp_ast =
  | Sig_ast of sig_item_t
  | Struct_ast of str_item_t


(* the type for the write_tag entry in the tag_functions record *)
type write_tag_t = loc_t -> string -> unit


(* The common interface of the vi and emacs tagging modules is captured
 * in this record. It contains the functions necessary for tagging.
 *)
type tag_functions = {
  (* start_unit file
   * start tagging compilation unit file
   *)
  start_unit : string -> unit;

  (* write_tag loc tag
   * write tag tag at loc
   *)
  write_tag : write_tag_t;

  (* finish one compilation unit *)
  finish_unit : unit -> unit;

  (* finish all tagging *)
  finish_tagging : unit -> unit;
}


(* Exception to wrap an error message and a location to be reported by
 * otags before continuing on the next input file. Lots of other
 * exceptions are transformed into Otags_parsing_error's, these are
 * those exceptions from which otags can recover. Therefore, when this
 * exceptions is raised, the internal state must be in a relatively
 * good shape. After this exception is caught the tags for the current
 * file are written to output, therefore no half tags entry should be
 * laying around somewhere. 
 *)
exception Otags_parsing_error of loc_t * string
