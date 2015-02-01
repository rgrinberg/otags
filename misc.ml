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
 * $Id: misc.ml,v 1.9 2012/05/21 09:29:29 tews Exp $
 * 
 * some misc functions
 * 
 *)

module U = Unix
module UL = Unix.LargeFile

open Global


let the = function
  | Some x -> x
  | None -> assert false


(* return true if s2 is an initial substring of s1 *)
let starts_with s1 s2 =
  let s1_len = String.length s1 in
  let s2_len = String.length s2 in
  if s1_len >= s2_len then
    (String.sub s1 0 s2_len) = s2
  else false


let module_name file_name = 
  let base = Filename.basename file_name in
  let module_name =
    if Filename.check_suffix base ".ml"
    then Filename.chop_suffix base ".ml"
    else if Filename.check_suffix base ".mli"
    then Filename.chop_suffix base ".mli"
    else base
  in
  String.capitalize module_name


(** Split string [s] at occurrences of [c]. Return the list of (non-zero)
    strings between sequences of [c].

    @param c split character
    @param s string to split
*)
let string_split c s =
  let len = String.length s in
  let rec iter i res =
    if i >= len 
    then List.rev res
    else
      let j =
	try String.index_from s i c 
	with Not_found -> len
      in
      iter (j + 1)
	(if i = j then res
	 else (String.sub s i (j - i)) :: res)
  in
  iter 0 []


(** Strip spaces and tabs at start and end of the argument. *)
let strip_white_space s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t') do
    decr j
  done;
  String.sub s !i (!j - !i + 1)


(* cut_out in_channel start end 
 * cuts the string from start - end out of in_channel
 *)
let cut_out inc start_pos end_pos =
  (* 
   * 3.11 FIX
   * work around out-of-file end positions in 3.11
   * this is fixed in 3.12
   * let end_pos = min end_pos (in_channel_length inc) in
   *)
  let len = end_pos - start_pos in
  (* Printf.eprintf "CUTOUT %d %d %d\n%!" start_pos end_pos len; *)
  let buf = String.create len
  in
    seek_in inc start_pos;
    really_input inc buf 0 len;
    buf


let input_line_at ic pos =
  seek_in ic pos;
  input_line ic



exception Skip_entry


(** Check whether the first argument is a directory. Raise
    {!Skip_entry} for errors. Errors are reported if the second argument
    is true and depending on {!Global.silent} and {!Global.verbose}.
*)
let is_directory f explicitly_listed =
  try
    (UL.stat f).UL.st_kind = U.S_DIR
  with
    | U.Unix_error(error, _, _) ->
      if explicitly_listed && (not !silent) || !verbose then
	Printf.eprintf "stat failure on %s: %s\n"
	  f
	  (U.error_message error);
      exit_status := 1;
      raise Skip_entry
