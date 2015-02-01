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
 * $Id: emacs.ml,v 1.12 2012/05/18 19:12:11 tews Exp $
 * 
 * write emacs tags files
 * 
 *)

open Global
open Types
open Source_channel

module Loc = Camlp4.PreCast.Loc


(* For files with INCLUDE or line directives we suddenly get tags with
 * locations from different files. We cannot tag with the main source file
 * because, for the line directives case, we don't know their location and,
 * for the INCLUDE case, they do not occur there. We therefore maintain 
 * several buffers to which we append tags. They are created as needed 
 * and stored in a hash table, because, for the line directive case, 
 * it is possible that one jumps several times between two files.
 * 
 * This buffers hash table is empty between compilation units. And the 
 * current buffer is some default empty buffer that is never used.
 * 
 * If file = "" then the current_buf, file and ic fields hold only
 * placeholders. Therefore filenames must be different from "".
 *)
type emacs_tag_state = {
  tags_oc : out_channel;
  buffers : (string, Buffer.t) Hashtbl.t;
  mutable current_buf : Buffer.t;
  mutable file : string;
}


(* Dig out the buffer into which the tags for file are written, to
 * append the next tag to the right buffer. If that buffer does not yet
 * exist it is created. 
 *)
let make_current_buffer es file =
  let buf =
    try
      Hashtbl.find es.buffers file
    with
      | Not_found -> 
	let buf = Buffer.create 4095 in
	Hashtbl.add es.buffers file buf;
	buf
  in
  es.current_buf <- buf;
  es.file <- file


let emacs_tag_line line tag line_number char =
  Printf.sprintf "%s\127%s\001%d,%d\n" line tag line_number char


let start_unit es file =
  assert(file <> "");
  assert(Hashtbl.length es.buffers = 0);
  let mod_name = Misc.module_name file in
  let module_tag = emacs_tag_line "" mod_name 1 0 in
  make_current_buffer es file;
  Buffer.add_string es.current_buf module_tag


let write_tag es loc tag = 
  (* 
   * Printf.eprintf "%s: def %s line %d: %s\n"
   *   (Loc.to_string loc)
   *   (Misc.cut_out (get_channel loc) (Loc.start_off loc) (Loc.stop_off loc))
   *   (Loc.start_bol loc)
   *   (Misc.cut_out (get_channel loc) (Loc.start_bol loc) (Loc.stop_off loc));
   *)
  if es.file <> Loc.file_name loc 
  then 
    make_current_buffer es (Loc.file_name loc);
  Buffer.add_string es.current_buf 
    (emacs_tag_line
       (Misc.cut_out (get_channel loc) (Loc.start_bol loc) (Loc.stop_off loc))
       tag
       (Loc.start_line loc)
       (Loc.start_bol loc))


  (* Buffer, never to be really used, serves as a placeholder for the
   * current_buf field between compilation units.
   *)
let default_empty_buffer = Buffer.create 1


let finish_unit es () =
  Hashtbl.iter 				(* XXX don't write empty buffers *)
    (fun file buf ->
      Printf.fprintf es.tags_oc "\012\n%s,%d\n" 
	(if !relative_file_prefix <> "" && Filename.is_relative file
	 then Filename.concat !relative_file_prefix file
	 else file)
	(Buffer.length buf);
      Buffer.output_buffer es.tags_oc buf;
    )
    es.buffers;
  Hashtbl.clear es.buffers;
  es.current_buf <- default_empty_buffer;
  es.file <- "";
  ()
  

let finish_tagging _es () = ()


let init oc =
  let es = {
    tags_oc = oc;
    buffers = Hashtbl.create 23;
    current_buf = default_empty_buffer;
    file = "";
  }
  in {
    start_unit = start_unit es;
    write_tag = write_tag es;
    finish_unit = finish_unit es;
    finish_tagging = finish_tagging es;
  }
