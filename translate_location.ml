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
 * $Id: translate_location.ml,v 1.2 2012/02/17 13:06:25 tews Exp $
 * 
 * translate locations after line directives
 * 
 *)

open Monitor_line_directive
open Types
open Source_channel

module Loc = Camlp4.PreCast.Loc


let line_directive_array = ref [| |]
let current_index = ref(-1)

let prepare_line_directives () =
  line_directive_array := Array.of_list (List.rev !parsed_line_directives);
  parsed_line_directives := [];
  current_index := -1;
  assert(
    snd(Array.fold_left
	  (fun (prev_off, res) ld -> 
	    (ld.ld_parse_offset, 
	     res && prev_off < ld.ld_parse_offset && ld.ld_file_offset = -1))
	  (0, true)
	  !line_directive_array))

let rec search_directive_array parse_off si ei =
  assert(parse_off >= !line_directive_array.(si).ld_parse_offset &&
	   (ei + 1 = Array.length !line_directive_array ||
	       parse_off < !line_directive_array.(ei + 1).ld_parse_offset));
  if si = ei then si
  else
    let middle = (si + ei + 1) / 2 in
    if parse_off < !line_directive_array.(middle).ld_parse_offset
    then search_directive_array parse_off si (middle - 1)
    else search_directive_array parse_off middle ei
    

let get_directive_index parse_off =
  if Array.length !line_directive_array = 0 ||
    parse_off < !line_directive_array.(0).ld_parse_offset 
  then -1
  else if !current_index >= 0 &&
      !line_directive_array.(!current_index).ld_parse_offset <= parse_off &&
      (!current_index + 1 = Array.length !line_directive_array ||
	  parse_off < 
	  !line_directive_array.(!current_index + 1).ld_parse_offset)
  then
    !current_index
  else begin
    current_index := 
      search_directive_array parse_off 0
      (Array.length !line_directive_array - 1);
    (* Printf.eprintf "GDI search %d gives %d\n%!" parse_off !current_index; *)
    !current_index
  end

let get_line_offset file_name line =
  (* Printf.eprintf "GLO %s line %d\n%!" file_name line; *)
  let loc = Loc.of_tuple (file_name, line, 0, 0, line, 0, 0, true) in
  let ic = get_channel loc in
  let line_count = ref 1 in
  let pos = ref 0 in
  seek_in ic 0;
  try
    while !line_count < line do
      if input_char ic = '\n'
      then incr line_count;
      incr pos;
    done;
    !pos
  with 
    | End_of_file -> raise(Otags_parsing_error(loc, "Invalid location"))

let source_line_offset directive =
  if directive.ld_file_offset >= 0 then directive.ld_file_offset
  else 
    let file_offset = get_line_offset directive.ld_file directive.ld_line in
    directive.ld_file_offset <- file_offset;
    file_offset

let translate_loc loc =
  let stop_parse_off = Loc.stop_off loc in
  let stop_dir_index = get_directive_index stop_parse_off in
  if stop_dir_index = -1 
  then begin
    (* Printf.eprintf "TL stop -1\n%!"; *)
    loc
  end
  else
    let stop_dir = !line_directive_array.(stop_dir_index) in
    let stop_diff = stop_dir.ld_parse_offset - (source_line_offset stop_dir) in
    let stop_off = stop_parse_off - stop_diff in
    let stop_bol = (Loc.stop_bol loc) - stop_diff in
    let start_parse_off = Loc.start_off loc in
    let start_dir_index = get_directive_index start_parse_off in
    let (start_off, start_bol) =
      if start_dir_index = -1 
      then (start_parse_off, Loc.start_bol loc)
      else
	let start_dir = !line_directive_array.(start_dir_index) in
	let start_diff = 
	  start_dir.ld_parse_offset - (source_line_offset start_dir) in
	let start_off = start_parse_off - start_diff in
	let start_bol = (Loc.start_bol loc) - start_diff in
	(start_off, start_bol)
    in
    let (file_name, start_line, _bol1, _off1, stop_line, _bol2, _off2, ghost) =
      Loc.to_tuple loc
    in
    (* 
     * Printf.eprintf "TL start index %d stop index %d\n%!" 
     *   start_dir_index stop_dir_index;
     *)
    Loc.of_tuple (file_name, start_line, start_bol, start_off, 
		  stop_line, stop_bol, stop_off, ghost)
