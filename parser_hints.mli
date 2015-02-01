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
 * $Id: parser_hints.mli,v 1.1 2012/05/21 09:29:29 tews Exp $
 * 
 * parser hints functionality
 * 
 *)


(** Parse and record the parser hints in all files and directories in
    the argument list.
*)
val process_parser_hints : string list -> unit


(** Lookup parser hints. Raise Not_found if no hints have been recorded. *)
val parser_hint : string -> (bool * string list)
