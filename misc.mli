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
 * $Id: misc.mli,v 1.6 2012/05/21 09:29:29 tews Exp $
 * 
 * some misc functions
 * 
 *)

(* Returns the contents of the option, asserting that the 
 * argument is not None.
 *)
val the : 'a option -> 'a

(* starts_with big small
 * returns true if small is an initial substring of big
 *)
val starts_with : string -> string -> bool


(* return the module name for a given file name *)
val module_name : string -> string


(** Split string [s] at occurrences of [c]. Return the list of (non-zero)
    strings between sequences of [c].

    @param c split character
    @param s string to split
*)
val string_split : char -> string -> string list


(** Strip spaces and tabs at start and end of the argument. *)
val strip_white_space : string -> string


(* cut_out ic start stop returns the part from start to stop (exclusive) 
 * from the file ic
 *)
val cut_out : in_channel -> int -> int -> string


(* input_line_at ic pos
 * inputs one line like input_line starting at pos in ic
 *)
val input_line_at : in_channel -> int -> string


exception Skip_entry

(** Check whether the first argument is a directory. Errors are
    reported if the second argument is true and depending on
    {!Global.silent} and {!Global.verbose}.
*)
val is_directory : string -> bool -> bool
