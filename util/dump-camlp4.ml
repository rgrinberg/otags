(* Standalone utility to dump the Camlp4 AST in a readable form.
 * Use camlp4 to parse stdin and save the AST in file dump-file.
 * Next a toplevel is started to read that file and use the 
 * toplevel printer to print the AST.
 *)


let standard_syntax = ref true

let parse_impl = ref true

let ocaml_path = ref ""

let verbose = ref false

let dump_file = ref None

let print_locations = ref true

let input_file = ref None

let arguments = Arg.align [
  ("-r", Arg.Clear standard_syntax,
   " parse revised syntax");
  ("-i", Arg.Clear parse_impl,
   " parse interface");
  ("-ocaml", Arg.Set_string ocaml_path,
   "path set the path to the ocaml bin-dir");
  ("-f", Arg.String(fun f -> input_file := Some f),
   "input-file parse input-file (instead of stdin)");
  ("-dump", Arg.String(fun f -> dump_file := Some f),
   "dump-file print ast from dump-file instead of parsing input");
  ("-noloc", Arg.Clear print_locations,
   " don't print locations");
  ("-v", Arg.Set verbose,
   " verbose");
]

let usage = "dump-camlp4"

let _ = Arg.parse arguments (fun _ -> assert false) usage
  
let camlp4_name = match !standard_syntax with
  | true -> "camlp4o"
  | false -> "camlp4r"

let camlp4_name = Filename.concat !ocaml_path camlp4_name

let mode_flag = match !parse_impl with
  | true -> "-impl"
  | false -> "-intf"

let input_source = match !input_file with
  | Some f -> f
  | None -> "-"

let parse_command = 
  camlp4_name ^ " " ^ mode_flag ^ " " ^ input_source 
  ^ " -printer d > camlp4-dump"

let _ = 
  match !dump_file with
    | None ->
      if !verbose then
	Printf.eprintf "Run: %s\n%!" parse_command;
      ignore(Sys.command parse_command)
    | Some f ->
      if !verbose then
	Printf.eprintf "don't parse any input, use dump in %s\n%!" f


let ast_file = match !dump_file with
  | None -> "camlp4-dump"
  | Some f -> f

let loc_printer =
  if !print_locations 
  then
    "let loc_printer ff loc =
       Format.fprintf ff \"[%s: sl %d(%d) so %d el %d(%d) eo %d%s]\"
	 (Loc.file_name loc)
	 (Loc.start_line loc) (Loc.start_bol loc) (Loc.start_off loc)
	 (Loc.stop_line loc) (Loc.stop_bol loc) (Loc.stop_off loc)
	 (if Loc.is_ghost loc then \" GHOST\" else \" REAL\");;"
  else
    "let loc_printer ff (loc : Loc.t) = Format.pp_print_string ff \"-\";;"


(* 
 * #print_depth 1073741823;;
 * #print_length 1073741823;;
 * #load "dynlink.cma";; 
 * #load "camlp4lib.cma";;
 * open Camlp4.PreCast;;
 * let loc_printer ff (loc : Loc.t) = Format.pp_print_string ff "-"
 * let loc_printer ff loc =
 *        Format.fprintf ff "[%s: sl %d(%d) so %d el %d(%d) eo %d%s]"
 * 	 (Loc.file_name loc)
 * 	 (Loc.start_line loc) (Loc.start_bol loc) (Loc.start_off loc)
 * 	 (Loc.stop_line loc) (Loc.stop_bol loc) (Loc.stop_off loc)
 * 	 (if Loc.is_ghost loc then " GHOST" else " REAL");;
 * #install_printer loc_printer;;
 * open Camlp4.PreCast.Ast;;
 * let buf = String.create 14
 * let ic = open_in "../camlp4-dump";;
 * really_input ic buf 0 14;
 * (input_value ic : Ast.str_item);;
 *)


let input = 
  "#print_depth 1073741823;;
   #print_length 1073741823;;
   #load \"dynlink.cma\";; 
   #load \"camlp4lib.cma\";;
   open Camlp4.PreCast;;
  " 
  ^ loc_printer ^ "\n"
  ^
  (* #directory "+compiler-libs";; *)
  "#install_printer loc_printer;;
   open Camlp4.PreCast.Ast;;
   let buf = String.create 14 in
   let ic = open_in \""
    ^ ast_file ^
   "\" in
   really_input ic buf 0 14;
   (input_value ic : Ast."
    ^
    (if !parse_impl then "str_item"
     else "sig_item")
    ^ ");;
"

let ocaml_command = 
  "echo '" ^ input ^ "' | " ^ (Filename.concat !ocaml_path "ocaml")
;;

if !verbose then
  Printf.eprintf "Run: %s\n%!" ocaml_command;
Sys.command ocaml_command


(*** Local Variables: ***)
(*** compile-command: "ocamlopt.opt dump-camlp4.ml -o dump-camlp4" ***)
(*** End: ***)
