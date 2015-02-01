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
 * $Id: parser_hints.ml,v 1.1 2012/05/21 09:29:29 tews Exp $
 * 
 * parser hints functionality
 * 
 *)

module U = Unix

open Misc
open Camlp4_names


let hints = Hashtbl.create 2039

let try_split_last_colon line =
  let i = ref ((String.length line) - 1) in
  while !i >= 0 && (line.[!i] = ' ' || line.[!i] = '\t') do
    decr i
  done;
  if !i >= 0 && line.[!i] = ':'
  then Some(String.sub line 0 !i)
  else None

let normalize_parser_list parsers =
  List.fold_left
    (fun (internal, res) new_parser ->
      try
	(internal, res @ (normalize_parser res new_parser))
      with
	| Not_found -> (false, res @ [new_parser])
    )
    (true, [])
    parsers



let process_hints_file ic file =
  let parser_info = ref (true, []) in
  try
    while true do
      let line = input_line ic in
      if String.length line > 0 && line.[0] <> '#' 
      then
	match try_split_last_colon line with
	  | Some line_without_colon ->
	    parser_info := 
	      normalize_parser_list (string_split ' ' line_without_colon)
	  | None ->
	    if snd !parser_info = [] 
	    then begin
	      Printf.eprintf 
		("Parser hints error in %s:\n" ^^
		    "First non-empty line must specify a parser list\n")
		file;
	      exit 2;
	    end
	    else
	      Hashtbl.add hints (strip_white_space line) !parser_info
    done
  with
    | End_of_file -> ()	    
		

let rec process_parser_hint file =
  if is_directory file true
  then process_parser_hint_dir file
  else 
    try
      let ic = open_in file in
      process_hints_file ic file;
      close_in ic
    with
      | Sys_error sys_msg ->
	Printf.eprintf "Cannot read %s\n%!" sys_msg;
	exit 2

and process_parser_hint_dir dir =
  let handle = U.opendir dir in
  let not_finished = ref true in
  while !not_finished do
    match
      try Some(U.readdir handle) with End_of_file -> None
    with
      | Some entry ->
	if entry = Filename.current_dir_name ||
	  entry = Filename.parent_dir_name
	then ()
	else process_parser_hint (Filename.concat dir entry)
      | None ->
	not_finished := false
  done
	

let process_parser_hints =
  List.iter process_parser_hint


let parser_hint file = Hashtbl.find hints file
