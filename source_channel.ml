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
 * $Id: source_channel.ml,v 1.3 2012/05/22 06:16:46 tews Exp $
 * 
 * hold the in_channel of the source file
 * 
 *)


open Types				(* for Otags_parsing_error *)
module Loc = Camlp4.PreCast.Loc


let current_file_name = ref ""

let current_in_channel = ref stdin

let reset () =
  if !current_file_name <> "" 
  then close_in !current_in_channel;
  current_file_name := ""

let open_file ?(primary_file = false) file_name loc =
  reset ();
  current_in_channel := 
    (try
       open_in file_name
     with
       | Sys_error sys_msg -> 
	 let msg = 
	   if primary_file
	   then "Cannot open " ^ sys_msg
	   else "Original source not available: " ^ sys_msg 
	 in
	 raise(Otags_parsing_error(loc, msg))
    );
  current_file_name := file_name


let get_channel ?primary_file loc =
  if Loc.file_name loc <> !current_file_name
  then open_file ?primary_file (Loc.file_name loc) loc;
  !current_in_channel
    


let full_string_of_loc loc =
  Printf.sprintf "[%s: sl %d(%d) so %d el %d(%d) eo %d%s]"
    (Loc.file_name loc)
    (Loc.start_line loc) (Loc.start_bol loc) (Loc.start_off loc)
    (Loc.stop_line loc) (Loc.stop_bol loc) (Loc.stop_off loc)
    (if Loc.is_ghost loc then " GHOST" else " REAL")
