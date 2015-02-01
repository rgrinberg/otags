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
 * $Id: otags.ml,v 1.33 2012/12/05 12:18:16 tews Exp $
 * 
 * main module with main function
 * 
 *)

open Conf
open Global
open Misc
open Types
open Source_channel
open Monitor_line_directive
open Translate_location
open Camlp4_names
open Parser_factory
open Parser_hints

module Loc = Camlp4.PreCast.Loc
module U = Unix
module UL = Unix.LargeFile


let parser_error_message loc msg =
  if not !silent then begin
    prerr_endline (Loc.to_string loc);
    prerr_endline msg
  end


let otags_dump_ast_module = ["-printer"; "otags_dump_ast.cma"]

let make_camlp4_command parser_list unit file =
  let (camlp4, rest_parsers) = camlp4_variant parser_list in
  let camlp4 = Filename.concat camlp4_exec_path camlp4 in
  let dir_options =
    List.fold_right 
      (fun d res -> "-I" :: d :: res)
      (camlp4_mod_path :: !camlp4_search_path)
      []
  in
  let parser_options = 
    List.fold_right (fun p res -> "-parser" :: p :: res) rest_parsers [] in
  let unit_option = match unit with
    | Signature -> "-intf"
    | Structure -> "-impl"
  in
  let options = 
    camlp4 :: dir_options 
    @ parser_options 
    @ otags_dump_ast_module
    @ [unit_option; file]
  in
  String.concat " " options


(* See also otags_dump_ast.ml *)
let read_camlp4_dump ic file unit =
  let magic = match unit with
    | Signature -> Conf.otags_camlp4_ast_intf_magic
    | Structure -> Conf.otags_camlp4_ast_impl_magic
  in
  try
    let input_magic = input_line ic in
    if input_magic = magic then 
      let line_directives = 
	(input_value ic : Monitor_line_directive.line_directive_record list) in
      let comp_unit =
	match unit with
	  | Signature -> Sig_ast(input_value ic : sig_item_t)
	  | Structure -> Struct_ast(input_value ic : str_item_t)
      in
      (line_directives, comp_unit)
    else begin
      Printf.eprintf 
	"External camlp4 has incompatible version. Reinstall otags.\n";
      exit 2
    end
  with
    | End_of_file ->
      Printf.eprintf "Error while reading the camlp4 ast for %s\n" file;
      raise Skip_entry
    
    
(* 
 * use a directive handler that aborts tagging on every directive,
 * see #5127
 * 
 * let directive_handler x = Some x
 *)

module PCAst = Camlp4.PreCast.Syntax.Ast

let directive_error loc =
  if not !silent then begin
    Printf.eprintf "Toplevel directive found in %s. Skip file.\n%!"
      (Loc.file_name loc);
    exit_status := 1;
  end;
  raise Skip_entry	

let sig_directive_handler int_dir =
  directive_error (PCAst.loc_of_sig_item int_dir)

let str_directive_handler str_dir =
  directive_error (PCAst.loc_of_str_item str_dir)


let parse_file_internally parser_list unit file =
  let pa = update_syntax parser_list in
  if !verbose then
    Printf.eprintf "Process %s internally as %s with parsers %s\n" 
      file 
      (string_of_unit_type unit)
      (short_string_of_parser_list parser_list);
  let start_loc = pa.mkloc file in
  let ic = 
    try get_channel ~primary_file:true start_loc 
    with
      | Otags_parsing_error(_loc, msg) ->
	if not !silent then 
	  prerr_endline msg;
	exit_status := 1;
	raise Skip_entry	
  in
  let stream = Stream.of_channel ic in
  match unit with
    | Signature -> 
      (try
	 Sig_ast(pa.parse_interf 
		   ~directive_handler:sig_directive_handler start_loc stream)
       with
	 | Otags_parsing_error(loc, msg) ->
	   parser_error_message loc msg;
	   exit_status := 1;
	   Tags.empty_sig_ast
      )
    | Structure -> 
      (try
	 let ast = 
	   pa.parse_implem 
	     ~directive_handler:str_directive_handler start_loc stream in
	 (* 
          * let module Dump = 
	  *       Camlp4.Printers.DumpCamlp4Ast.Make(Camlp4.PreCast.Syntax)
	  * in
	  * Dump.print_implem ~input_file:file ~output_file:"camlp4-dump" ast;
          *)
	 Struct_ast ast
       with
	 | Otags_parsing_error(loc, msg) ->
	   parser_error_message loc msg;
	   exit_status := 1;
	   Tags.empty_str_ast
      )


let parse_file_externally parser_list unit file = 
  let camlp4 = make_camlp4_command parser_list unit file in
  if !verbose then begin
    Printf.eprintf "Parse %s externally as %s with parsers %s\n"
      file 
      (string_of_unit_type unit)       
      (short_string_of_parser_list parser_list);
    prerr_endline camlp4;
  end;
  let ast_ic = U.open_process_in camlp4 in
  let directives_and_unit = read_camlp4_dump ast_ic file unit in
  match U.close_process_in ast_ic with
    | U.WEXITED 0 -> directives_and_unit
    | _ -> 
      if not !silent then
	Printf.eprintf "External camlp4 on %s failed\n" file;
      exit_status := 1;
      raise Skip_entry


let process_file tagfun unit file =
  let (parse_internal, parser_list) =
    try parser_hint file 
    with Not_found -> (!use_internal_parsers, !user_parser_list)
  in
  if parser_list = [] then begin
    if not !silent then 
      Printf.eprintf "Parser list empty for %s\n%!" file;
    exit 2
  end;
  (* let pa_start = Unix.gettimeofday() in *)
  let comp_unit = match parse_internal && !use_internal_parsers with
    | true -> parse_file_internally parser_list unit file
    | false -> 
      let (line_directives, comp_unit) = 
	parse_file_externally parser_list unit file in
      parsed_line_directives := line_directives;
      comp_unit
  in
  prepare_line_directives ();
  tagfun.start_unit file;
  (try
     Tags.generate_tags tagfun.write_tag comp_unit
   with
     | Otags_parsing_error(loc, msg) ->
       parser_error_message loc msg;
       exit_status := 1;
  );
  tagfun.finish_unit();
  (* 
   * Printf.printf "process %s in %.2f ms\n"
   *   file
   *   ((Unix.gettimeofday() -. pa_start) *. 1000.0);
   *)
  ()

(* process file or directory f, generating output for tags_out_ref.
 * If subdir_ref_option <> None then process_entry has been called
 *      recursively from process_directory. In this case it must append 
 *      detected directories to the reference in subdir_ref_option. These 
 *      directories will be processed by process_directory later.
 * If subdir_ref_option = None then process_entry has been called
 *      from somewhere higher up. In this case it must invoke 
 *      process_directory 
 *      when it detects an directory (and option -r was present).
 *)
let rec process_entry tagfun f subdir_ref_option =
  if Filename.check_suffix f ".ml"
  then process_file tagfun Structure f
  else if Filename.check_suffix f ".mli"
  then process_file tagfun Signature f
  else if !recurse_subdirectories && is_directory f (subdir_ref_option = None)
  then 
    match subdir_ref_option with
      | None -> process_directory tagfun f
      | Some r -> 
	(* process_directory tagfun tags_out f *)
	r := f :: !r
  else if ((subdir_ref_option = None) && (not !silent)) || !verbose
  then begin
    Printf.eprintf
      "File \"%s\"\nSkip file because extension is not recognized\n"
      f;
    exit_status := 1;
  end
  else ()

and process_directory tagfun subdir =
  if !verbose then
    Printf.eprintf "Descend into directory %s\n" subdir;
  let subdirs = ref [] in
  let subdir_ref_option = Some subdirs in
  let handle = U.opendir subdir in
  let subdir_concat =
    if subdir = Filename.current_dir_name then "" else subdir in
  let not_finished = ref true in
  while !not_finished do
    match 
      try Some(U.readdir handle) with End_of_file -> None
    with
      | Some current_or_parent 
	  when current_or_parent = Filename.current_dir_name
	  || current_or_parent = Filename.parent_dir_name
	    -> ()
      | Some entry ->
	(try
	   process_entry tagfun
	     (Filename.concat subdir_concat entry)
	     subdir_ref_option	
	 with
	   | Skip_entry -> ()
	   | e -> 
	     if not !silent then
	       Printf.eprintf "Escaping exception during processing %s\n"
		 (Filename.concat subdir entry);
	     raise e	
	)
      | None ->
	not_finished := false
  done;
  List.iter (process_directory tagfun) !subdirs



let switch_to_external_parser () =
  use_internal_parsers := false;
  switched_to_external_by_hand := true;
  if !verbose then
    prerr_endline "Switch to external mode"


let switch_to_internal_parser () =
  use_internal_parsers := true;
  if !verbose then
    prerr_endline "Switch to internal mode"


let print_parser_list () =
  if !verbose then flush stderr;
  Printf.printf "Current parser list: %s\n%!"
    (String.concat ", " !user_parser_list)


let reset_parser_list () =
  user_parser_list := default_parser_list;
  if !verbose then
    prerr_endline "Reset parser list to default";
  if !switched_to_external_by_hand = false && !use_internal_parsers = false
  then begin
    use_internal_parsers := true;
    if !verbose then
      prerr_endline "Switch back to internal mode";
  end


(* Add command line argument parser_name to the parser list. Use 
 * canonical camlp4 names only and obey dependencies. Do not add a 
 * given parser more than once.
 *)
let add_parser parser_name =
  let use_internal_before = !use_internal_parsers in
  let parsers_to_add = ref []
  in
    (try
       parsers_to_add := normalize_parser !user_parser_list parser_name
     with
       | Not_found ->
	   use_internal_parsers := false;
	   parsers_to_add := [parser_name]
    );
    if !verbose then begin
      if !parsers_to_add = [] then
	prerr_endline "Add parser(s): all parsers already present in parser list"
      else
	Printf.eprintf "Add parser(s) %s\n" (String.concat " " !parsers_to_add);
      if use_internal_before <> !use_internal_parsers then
	print_endline "Switch to external mode"
    end;
    user_parser_list := !user_parser_list @ !parsers_to_add


let clear_parser_list () =
  user_parser_list := [];
  if !verbose then
    prerr_endline "Clear parser list";
  if !switched_to_external_by_hand = false then begin
    use_internal_parsers := true;
    if !verbose then
      prerr_endline "Switch back to internal mode";
  end


(* Print version and exit *)
let print_version () =
  Printf.printf "otags version %s for ocaml %s.x compiled with ocaml %s\n"
    otags_version ocaml_version Sys.ocaml_version;
  exit 0
      
  
type otags_actions =
  | Clear_parser_list
  | Add_parser of string
  | Reset_parser_list
  | Print_parser_list
  | Use_internal_parsers
  | Use_external_parsers
  | Process_file of string * unit_type option


let action_list = ref []

let queue_action a () = 
  action_list := a :: !action_list

let queue_interface i = queue_action(Process_file(i, Some Signature))()

let queue_implementation i = queue_action(Process_file(i, Some Structure))()


let add_to_camlp4_search_path d =
  camlp4_search_path := !camlp4_search_path @ [d]


let add_parser_hints f =
  parser_hints_list := f :: !parser_hints_list

let anon_fun s = queue_action(Process_file(s, None)) ()

let arguments = Arg.align [
  ("-r", Arg.Set recurse_subdirectories,
   " descend recursively into directories");
  ("-o", Arg.String (fun f -> tags_file_name := Some f),
   "file output file [default TAGS for Emacs and tags for vi]");
  ("-a", Arg.Set append_to_tags_file,
   " append to an existing TAGS file");
  ("-vi", Arg.Clear emacs_mode,
   " generate tags for vi");
  ("-intf", Arg.String queue_interface,
   "file tag file as an interface");
  ("-impl", Arg.String queue_implementation,
   "file tag file as an implementation");
  ("-pc", Arg.Unit(queue_action Clear_parser_list),
   " clear parser list");
  ("-pa", Arg.String(fun s -> queue_action (Add_parser s) ()),
   "parser add parser to parser list");
  ("-pr", Arg.Unit(queue_action Reset_parser_list),
   " reset parser list to default [OCaml without extensions]");
  ("-pp", Arg.Unit(queue_action Print_parser_list),
   " print current parser list");
  ("-I", Arg.String add_to_camlp4_search_path,
   "dir add directory dir to the camlp4 search path");
  ("-add-path", Arg.Set_string relative_file_prefix,
   "path prepend path to relative file names in tags file");
  ("-parser-hints", Arg.String add_parser_hints,
   "file process parser hints in file");
  ("-intern", Arg.Unit(queue_action Use_internal_parsers),
   " switch to internal mode [internal mode is default]");
  ("-extern", Arg.Unit(queue_action Use_external_parsers),
   " switch to external mode");
  ("-version", Arg.Unit print_version,
   " print version and exit");
  ("-v", Arg.Unit(fun () -> verbose := true; silent := false),
   " be more verbose");
  ("-q", Arg.Unit(fun () -> verbose := false; silent := true),
   " be quiet");
]


let usage_message =
  Printf.sprintf 
    "Usage %s [arguments...]\n\
     Creates tags files for Emacs or vi[m] from OCaml sources.\n\
     Options and file arguments can be mixed. Order matters for many options.\n\
     The options -r, -o, -a, -vi, -I, -parser-hints, -add-path, -v and -q\n\
     have a global effect regardless of their position. The options -pc,\n\
     -pa, -pr, -pp, -extern and -intern affect only file arguments which \n\
     follow them.\n\n\
     Recognized options:"
    Sys.argv.(0)


let run_action tagfun = function
  | Clear_parser_list -> clear_parser_list () 
  | Add_parser parser_name -> add_parser parser_name
  | Reset_parser_list -> reset_parser_list ()
  | Print_parser_list -> print_parser_list ()
  | Use_internal_parsers -> switch_to_internal_parser ()
  | Use_external_parsers -> switch_to_external_parser ()
  | Process_file(file, unit_option) -> 
    try
      match unit_option with
	| None -> process_entry tagfun file None
	| Some unit -> process_file tagfun unit file
    with
      | Skip_entry -> ()
      | e -> 
	if not !silent then
	  Printf.eprintf "Escaping exception during processing %s\n" file;
	raise e


let main () =
  Arg.parse arguments anon_fun usage_message;
  if !append_to_tags_file && !emacs_mode = false then begin
    if not !silent then
      prerr_endline 
	"Appending to tags files is only supported for emacs TAGS files!";
    exit 2;
  end;
  process_parser_hints !parser_hints_list;
  let output_name = match !tags_file_name with
    | Some f -> f
    | None -> match !emacs_mode with
	| true -> "TAGS"
	| false -> "tags"
  in
  let tags_oc = 
    if output_name = "-"
    then stdout
    else
      open_out_gen 
	(if !append_to_tags_file 
	 then [Open_append; Open_creat; Open_text]
	 else [Open_wronly; Open_trunc; Open_creat; Open_text])
	0o666 output_name
  in
  let tagfun = 
    if !emacs_mode 
    then Emacs.init tags_oc
    else Vi.init tags_oc
  in
  List.iter 
    (run_action tagfun)
    (List.rev !action_list);
  tagfun.finish_tagging();
  close_out tags_oc


let main_ex () =
  try
    if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-v" then
      Printexc.record_backtrace true;
    main ();
    exit !exit_status
  with
    | e -> 
      let backtrace = if !verbose then Printexc.get_backtrace() else "" in
      prerr_string "\nFatal error: escaping exception ";
      prerr_endline (Printexc.to_string e);
      (match e with
	| Loc.Exc_located(loc, oe) ->
	  prerr_endline "Breaking apart the located exception gives:";
	  prerr_endline (Loc.to_string loc);
	  prerr_endline (Printexc.to_string oe);
	  prerr_endline "Try ErrorHandler on the located exception:";
	  prerr_endline (Camlp4.ErrorHandler.to_string oe);
	| U.Unix_error(error, _func, _info) ->
	  Printf.eprintf "%s\n" (U.error_message error)      
	| _ -> ()
      );
      prerr_endline "";
      if Printexc.backtrace_status() then begin
	prerr_string backtrace;
	prerr_endline 
	  "\n\
           Please send the command line, the input files and the output\n\
           above as bug report to otags@askra.de";
      end
      else 
	prerr_endline 
	  "Please rerun otags with -v as *first* option to get a backtrace\n\
           and send the command line, the input files and the backtrace\n\
           as bug report to otags@askra.de";
      exit 3

;;

main_ex()
