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
 * $Id: vi.ml,v 1.9 2012/01/26 13:16:19 tews Exp $
 * 
 * write vi tags files
 * 
 *)

open Global
open Types
open Source_channel

module Loc = Camlp4.PreCast.Loc


type vi_tag = {
  tag : string;
  tag_file : string;
  address : string;
  (* To order the vi tags I store them in a ordered set, see 
   * module Tagset below. However, I would really need multi-sets,
   * because the same identifier might be tagged with several locations.
   * I therefore use tag_file and the character position to distinguish 
   * between the same tag that occurs at several locations.
   *)
  position : int;
}

module Ordered_tag = struct
  type t = vi_tag
  let compare tag_1 tag_2 = 
    let test_1 = compare tag_1.tag tag_2.tag in
    if test_1 <> 0 then test_1
    else
      let test_2 = compare tag_1.tag_file tag_2.tag_file in
      if test_2 <> 0 then test_2
      else
	compare tag_1.position tag_2.position
end

module Tagset = Set.Make(Ordered_tag)

(* 
 * module Tagset = struct
 *   type t = vi_tag list
 *   let add el t = el :: t
 *   let iter f t = List.iter f (List.rev t)
 *   let empty = []
 * end
 *)


(* State record for vi tags table generation. Most importantly 
 * the state contains the ordered set of all tags, to which tags are 
 * added for the whole livetime of the program. Only at the end, when all 
 * source files have been read, this set is written into the tags file
 * at tags_oc. 
 *)
type vi_state = {
  tags_oc : out_channel;
  mutable tags : Tagset.t;
}


let sorted_vi_tags_header =
  "!_TAG_FILE_FORMAT	1	/without ;\"/\n\
   !_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted/\n"


(* escape all '/' and '\' in s *)
let ex_search_escape s =
  let len = String.length s in
  let n = ref len in
  for i = 0 to len - 1 do
    match s.[i] with
      | '/' -> incr n
      | '\\' -> incr n
      | _ -> ()
  done;
  if len = !n then s
  else
    let new_s = String.create !n in
    let j = ref 0 in
    for i = 0 to len - 1 do
      match s.[i] with
	| '/' -> 
	  new_s.[!j] <- '\\'; incr j;
	  new_s.[!j] <- '/'; incr j
	| '\\' ->
	  new_s.[!j] <- '\\'; incr j;
	  new_s.[!j] <- '\\'; incr j
	| c ->
	  new_s.[!j] <- c; incr j
    done;
    new_s
  

let ex_search_line line = 
  "/^" ^ (ex_search_escape line) ^ "$/"
;;

let write_vi_line oc tag =
  Printf.fprintf oc "%s\t%s\t%s;\n" 
    tag.tag 
    (if !relative_file_prefix <> "" && Filename.is_relative tag.tag_file
     then Filename.concat !relative_file_prefix tag.tag_file
     else tag.tag_file)
    tag.address


let add_tag vs tag =
  vs.tags <- Tagset.add tag vs.tags


let start_unit vs file =
  let mod_name = Misc.module_name file in
  add_tag vs {tag = mod_name; tag_file = file; address = "1"; position = 0}


let write_tag vs loc tag = 
  let loc_line = Misc.input_line_at (get_channel loc) (Loc.start_bol loc) in
  let address = 
    if String.length loc_line <= 1
    then string_of_int (Loc.start_line loc)
    else ex_search_line loc_line
  in
  add_tag vs {tag = tag; 
	      tag_file = Loc.file_name loc;
	      address = address;
	      position = Loc.start_off loc;
	     }


let finish_unit _vs () = ()

let finish_tagging vs () =
  output_string vs.tags_oc sorted_vi_tags_header;
  Tagset.iter (write_vi_line vs.tags_oc) vs.tags;
  vs.tags <- Tagset.empty


let init oc =
  let vs = {
    tags_oc = oc;
    tags = Tagset.empty;
  }
  in {
    start_unit = start_unit vs;
    write_tag = write_tag vs;
    finish_unit = finish_unit vs;
    finish_tagging = finish_tagging vs;
  }
