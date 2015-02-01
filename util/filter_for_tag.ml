(* This program reads a _tags file and classifies all files inside a
 * directory hierarchy according to some tags on the command line.
 * 
 * It links with some modules of ocamlbuild to load _tags files and
 * read the information therein.
 *)


open Ocamlbuild_pack


(* 
 * for the ocaml directory tree:
 * keep files with suffix
 *   .ml .mli .mlast .mlbuild .mlp .ml4
 * 
 * flush files with suffix
 *   .mll .mlpack .mllib .c 
 *   .S .h .s .depend .cvsignore .asm
 *   .ico .rc .boot README .in .mly .sh
 *   .clib .dlib .itarget
 * 
 * flush files that match pattern:
 *   "boot/ocaml" "/man/" emacs Makefile
 *)


let report_all = ref false

let suffixes = [".ml"; ".mli"; ".mlast"; ".mlbuild"; ".mlp"; ".ml4"]


let sort_file tags files entry =
  if !report_all 
    || List.exists (fun suf -> Filename.check_suffix entry suf) suffixes
  then
    let i = ref 0 in
    let tags_len = Array.length tags in
    let not_found = ref true in
    let entry_tags = Configuration.tags_of_filename entry in
    while !i < tags_len && !not_found do
      if Tags.mem tags.(!i) entry_tags 
      then begin
	files.(!i) <- entry :: files.(!i);
	not_found := false;
      end;
      incr i;      
    done;
    if !not_found then
      files.(!i) <- entry :: files.(!i)
    

let print_entry tag files =
  print_endline tag;
  List.iter
    (fun f -> print_string "  "; print_endline f)
    files


let print_sorted_entries tags files =
  for i = 0 to Array.length tags - 1 do
    print_entry tags.(i) files.(i)
  done;
  print_entry "unmatched" files.(Array.length files - 1)


module U = Unix
module UL = Unix.LargeFile
let is_directory f = (UL.stat f).UL.st_kind = U.S_DIR


let rec walk_directory file_fun base =
  let subdirs = ref [] in
  let handle = U.opendir base in
  let not_finished = ref true in
  while !not_finished do
    match 
      try Some(U.readdir handle) with End_of_file -> None
    with
      | Some ("." | "..") -> ()
      | Some entry ->
	let file = Filename.concat base entry in
	if is_directory file
	then
	  subdirs := file :: !subdirs
	else
	  file_fun file
      | None ->
	not_finished := false
  done;
  List.iter (walk_directory file_fun) !subdirs
  

let base_dir = ref None 

let tags_list = ref []


let anon_fun x = match !base_dir with
  | None -> base_dir := Some x
  | Some _ -> tags_list := x::!tags_list

let usage_message =
  Printf.sprintf 
    "Usage %s [option] ... directory tag ...\n\
     Recognized options are:"
    Sys.argv.(0)

let arguments = Arg.align [
  ("-all", Arg.Set report_all,
   " sort all files and not only those with interesting suffixes");
]


let main() =
  Arg.parse arguments anon_fun usage_message;
  if !base_dir = None || !tags_list = [] then begin
    Arg.usage arguments usage_message;
    exit 1;
  end;

  let base = match !base_dir with
    | Some x -> x
    | None -> assert false
  in
  let tags = Array.of_list !tags_list in
  let files = Array.make (1 + Array.length tags) [] in

  Configuration.parse_file ~dir:base (Filename.concat base "_tags");
  
  walk_directory (sort_file tags files) base;

  print_sorted_entries tags files;
  ()


let main_ex =
  try
    Printexc.record_backtrace true;
    main()
  with
    | e -> 
      let backtrace = Printexc.get_backtrace() in
      prerr_string "\nFatal error: escaping exception ";
      prerr_endline (Printexc.to_string e);
      prerr_string backtrace;
      exit 2
