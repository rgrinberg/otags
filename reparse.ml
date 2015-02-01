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
 * $Id: reparse.ml,v 1.14 2012/01/23 14:27:30 tews Exp $
 * 
 * location-parsing hack for those ast nodes that do not provide
 * sufficient location info
 * 
 *)

open Types				(* for Otags_parsing_error *)
open Source_channel

module Loc = Camlp4.PreCast.Loc


let report_exception loc f x =
  try
    f x
  with
    | End_of_file ->
      raise 
	(Otags_parsing_error(loc, 
			     "Invalid Camlp4 location yields internal error"))
    (* 
     * | e ->
     *   let bt = Printexc.get_backtrace () in
     *   prerr_endline "";
     *   prerr_endline (Loc.to_string _loc);
     *   prerr_string "Fatal reparse error: escaping exception ";
     *   prerr_endline (Printexc.to_string e);
     *   if bt = "" then
     * 	prerr_endline "No exception backtrace available"
     *   else begin
     * 	prerr_endline "";
     * 	prerr_string bt;
     *   end;
     *   raise e
     *)


(* Adjust position after skipping over one character forward. 
 * The skipped char must not be a newline
 *)
let incr_pos pos = {pos with Lexing.pos_cnum = pos.Lexing.pos_cnum + 1}


(* Adjust position to the previous character. Newlines are treated
 * correctly. The result is None when pos points already to the first 
 * character of pos. buf_start_pos must be the start position of buf. 
 * It may be needed for determining the start_of_line position.
 *)
let decr_pos buf start_buf buf_start_pos pos = 
  if pos.Lexing.pos_cnum <= start_buf 
  then None
  else if buf.[pos.Lexing.pos_cnum - start_buf - 1] = '\n'
  then
    let line_start =
      try
	start_buf + 1 +
	(String.rindex_from buf (pos.Lexing.pos_cnum - start_buf - 2) '\n')
      with
	| Invalid_argument _ 
	| Not_found 
	  -> buf_start_pos.Lexing.pos_bol
    in
    Some 
      {pos with 
	Lexing.pos_cnum = pos.Lexing.pos_cnum - 1;
	pos_lnum = pos.Lexing.pos_lnum - 1;
	pos_bol = line_start
      }
  else
    Some
      {pos with Lexing.pos_cnum = pos.Lexing.pos_cnum - 1}


(* Adjust position after skipping over a newline forward. *)
let incr_line pos = 
  {Lexing.pos_fname = pos.Lexing.pos_fname;
   pos_lnum = pos.Lexing.pos_lnum + 1;
   pos_bol = pos.Lexing.pos_cnum + 1;
   pos_cnum = pos.Lexing.pos_cnum + 1
  }


let init_reparse loc =
  let start_off = Loc.start_off loc in
  let stop_off = Loc.stop_off loc in
  (* 
   * Printf.eprintf "IR sl %s %d(%d) so %d el %s %d(%d) eo %d %s\n"
   *   (Loc.file_name loc)
   *   (Loc.start_line loc) (Loc.start_bol loc) (Loc.start_off loc)
   *   (Loc.file_name loc)
   *   (Loc.stop_line loc) (Loc.stop_bol loc) (Loc.stop_off loc)
   *   (if Loc.is_ghost loc then "ghost" else "real");
   *)
  let buf = Misc.cut_out (get_channel loc) start_off stop_off in
  (* Printf.eprintf "IR |%s|\n%!" buf; *)
  let start = Loc.start_pos loc in
  let stop = Loc.stop_pos loc 
  in
  (buf, start_off, start, stop)


let finish_reparse start stop =
  Loc.of_tuple (start.Lexing.pos_fname, (* file_name *)
		start.Lexing.pos_lnum, (* start_line *)
		start.Lexing.pos_bol, (* start_bol *)
		start.Lexing.pos_cnum, (* start_off *)
		stop.Lexing.pos_lnum, (* stop_line *)
		stop.Lexing.pos_bol, (* stop_bol *)
		stop.Lexing.pos_cnum, (* stop_off *)
		false (* ghost *)
  )
		  
		

let rec word_forward buf start_buf pos =
  if pos.Lexing.pos_cnum - start_buf >= String.length buf
  then (* leave buffer at the left hand side *)
    pos
  else
    match buf.[pos.Lexing.pos_cnum - start_buf] with
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9'
      | '_' -> word_forward buf start_buf (incr_pos pos)
      | _ -> pos


let rec space_forward buf start_buf pos = 
  match buf.[pos.Lexing.pos_cnum - start_buf] with
    | ' ' | '\t' -> space_forward buf start_buf (incr_pos pos)
    | '\n' -> space_forward buf start_buf (incr_line pos)
    | _ -> pos


let rec word_backward buf start_buf buf_start pos =
  match decr_pos buf start_buf buf_start pos with
    | None -> pos
    | Some pos_1 ->
      match buf.[pos_1.Lexing.pos_cnum - start_buf] with
	| 'A' .. 'Z'
	| 'a' .. 'z'
	| '0' .. '9'
	| '_' -> word_backward buf start_buf buf_start pos_1
	| _ -> pos


let rec space_backward buf start_buf buf_start pos =
  match decr_pos buf start_buf buf_start pos with
    | None -> pos
    | Some pos_1 ->
      match buf.[pos_1.Lexing.pos_cnum - start_buf] with
	| ' ' | '\t' | '\n' -> space_backward buf start_buf buf_start pos_1
	| _ -> pos


let opt_plus_minus_forward buf start_buf pos =
  match buf.[pos.Lexing.pos_cnum - start_buf] with
    | '+' 
    | '-' -> incr_pos pos
    | _ -> pos


(* moves forward to character c 
 * c must not be a newline
 *)
let rec forward_to c buf start_buf pos =
  match buf.[pos.Lexing.pos_cnum - start_buf] with
    | x when c = x -> pos
    | '\n' -> forward_to c buf start_buf (incr_line pos)
    | _ -> forward_to c buf start_buf (incr_pos pos)


(* moves forward after character c
 * c must not be a newline
 *)
let forward_after c buf start_buf pos =
  incr_pos(forward_to c buf start_buf pos)


let start_char_intern loc =
  let start_off = Loc.start_off loc in
  (* Printf.eprintf "REP START CHAR %s\n%!" (Loc.to_string loc); *)
  let buf = Misc.cut_out (get_channel loc) start_off (start_off + 1) in
  (* Printf.eprintf "REP START CHAR END\n%!"; *)
  buf.[0]

let start_char loc =
  report_exception loc start_char_intern loc


let stop_char_intern loc =
  let stop_off = Loc.stop_off loc in
  let buf = Misc.cut_out (get_channel loc) (stop_off - 1) stop_off in
  buf.[0]

let stop_char loc =
  report_exception loc stop_char_intern loc


let loc_of_nth_word_intern n loc =
  assert( n >= 1);
  let (buf, start_buf, start, _) = init_reparse loc in
  let rec move_n_forward start = function 
    | 1 -> start
    | n -> 
      let start = word_forward buf start_buf start in
      let start = space_forward buf start_buf start in
      move_n_forward start (n - 1)
  in
  let start = move_n_forward start n in
  let stop = word_forward buf start_buf start in
  finish_reparse start stop

let loc_of_nth_word n loc =
  report_exception loc (loc_of_nth_word_intern n) loc


let loc_of_first_word = loc_of_nth_word 1

let loc_of_second_word = loc_of_nth_word 2

let loc_of_third_word = loc_of_nth_word 3


let loc_without_first_word_intern loc =
  let (buf, start_buf, start, stop) = init_reparse loc in
  let start = word_forward buf start_buf start in
  let start = space_forward buf start_buf start in
  finish_reparse start stop

let loc_without_first_word loc =
  report_exception loc (loc_without_first_word_intern) loc


let loc_of_word_after_type_param_intern loc =
  let (buf, start_buf, start, _) = init_reparse loc in
  let start = opt_plus_minus_forward buf start_buf start in
  let start = space_forward buf start_buf start in
  let start = forward_after '\'' buf start_buf start in
  let start = space_forward buf start_buf start in
  let start = word_forward buf start_buf start in
  let start = space_forward buf start_buf start in
  let stop = word_forward buf start_buf start in
  finish_reparse start stop

let loc_of_word_after_type_param loc =
  report_exception loc (loc_of_word_after_type_param_intern) loc


let loc_of_word_after_paren_intern loc =
  let (buf, start_buf, start, _) = init_reparse loc in
  let start = forward_after ')' buf start_buf start in
  let start = space_forward buf start_buf start in
  let stop = word_forward buf start_buf start in
  finish_reparse start stop

let loc_of_word_after_paren loc =
  report_exception loc (loc_of_word_after_paren_intern) loc


let loc_of_first_word_after_word_intern word loc =
  let (buf, start_buf, start, _) = init_reparse loc in
  let start =
    if Misc.starts_with buf word
    then 
      let start = 
	{start with 
	  Lexing.pos_cnum = start.Lexing.pos_cnum + String.length word}
      in
      space_forward buf start_buf start
    else
      start
  in
  let stop = word_forward buf start_buf start in
  finish_reparse start stop

let loc_of_first_word_after_word word loc =
  report_exception loc (loc_of_first_word_after_word_intern word) loc


let loc_of_last_word_intern loc =
  let (buf, start_buf, buf_start, stop) = init_reparse loc in
  let start = word_backward buf start_buf buf_start stop in
  finish_reparse start stop

let loc_of_last_word loc =
  report_exception loc (loc_of_last_word_intern) loc


let loc_inside_parens_intern loc =
  let (buf, start_buf, buf_start, _) = init_reparse loc in
  assert(buf.[0] = '(');
  let start = incr_pos buf_start in
  let start = space_forward buf start_buf start in
  let paren_end = forward_to ')' buf start_buf start in
  let stop = space_backward buf start_buf buf_start paren_end in
  finish_reparse start stop

let loc_inside_parens loc =
  report_exception loc (loc_inside_parens_intern) loc

