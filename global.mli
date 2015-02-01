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
 * $Id: global.mli,v 1.13 2012/05/21 09:29:29 tews Exp $
 * 
 * global variables
 * 
 *)


val verbose : bool ref

val silent : bool ref

  (* whether to recursively search subdirectories *)
val recurse_subdirectories : bool ref

  (* whether to use internal or external mode *)
val use_internal_parsers : bool ref

  (* if true generate an emacs tags file, if false generate a vi one *)
val emacs_mode : bool ref

  (* name of the output file *)
val tags_file_name : string option ref

  (* whether to append to the tags file *)
val append_to_tags_file : bool ref

(** prefix added to relative files in tags output *)
val relative_file_prefix : string ref

  (* Whether some "-external" occured. Then "-pr" and "-pc" do not 
   * switch back to internal mode
   *)
val switched_to_external_by_hand : bool ref

val default_parser_list : string list

val user_parser_list : string list ref

val camlp4_search_path : string list ref

(** The exit status to use. *)
val exit_status : int ref

(** the list of parser hints arguments *)
val parser_hints_list : string list ref
