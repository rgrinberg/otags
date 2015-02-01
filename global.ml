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
 * $Id: global.ml,v 1.14 2012/05/22 07:45:44 tews Exp $
 * 
 * global variables
 * 
 *)


let verbose = ref false

let silent = ref false

let recurse_subdirectories = ref false

let use_internal_parsers = ref true

let emacs_mode = ref true

let tags_file_name = ref None

let append_to_tags_file = ref false

let relative_file_prefix = ref ""

  (* Whether some "-external" occured. Then "-pr" and "-pc" do not 
   * switch back to internal mode
   *)
let switched_to_external_by_hand = ref false

let default_parser_list = [
  "Camlp4OCamlRevisedParser";
  "Camlp4OCamlParser";
]


let user_parser_list = ref default_parser_list

let camlp4_search_path = ref []

let exit_status = ref 0

let parser_hints_list = ref []
