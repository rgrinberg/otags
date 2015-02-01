

module ID = struct
  let name = "otags_dump_ast - dump camlp4 ast with line directive info"
  let version = Conf.otags_version
end

module Make(Syntax : Camlp4.Sig.Camlp4Syntax) 
  : Camlp4.Sig.Printer(Syntax.Ast).S = 
struct
  include Syntax

  let with_open_out_file file_name_opt f =
    match file_name_opt with
      | Some file -> 
	let oc = open_out_bin file in
        f oc;
        flush oc;
        close_out oc
      | None -> 
	set_binary_mode_out stdout true; 
	f stdout; 
	flush stdout

  let print_interf ?input_file:_ ?output_file ast =
    with_open_out_file output_file
      (fun oc ->
	output_string oc Conf.otags_camlp4_ast_intf_magic;
	output_char oc '\n';
	output_value oc 
	  (!Monitor_line_directive.parsed_line_directives :
	      Monitor_line_directive.line_directive_record list);
	(* XXX One should specify here that the ast has type
	 * Camlp4.PreCast.Syntax.Ast.sig_item, because it is read with
	 * that type again.
	 *)
	output_value oc (ast : Syntax.Ast.sig_item))

  let print_implem ?input_file:_ ?output_file ast =
    with_open_out_file output_file
      (fun oc ->
	output_string oc Conf.otags_camlp4_ast_impl_magic;
	output_char oc '\n';
	output_value oc 
	  (!Monitor_line_directive.parsed_line_directives :
	      Monitor_line_directive.line_directive_record list);
	output_value oc (ast : Syntax.Ast.str_item))

end

module X = 
  Camlp4.Register.OCamlSyntaxExtension
    (ID)(Monitor_line_directive.Line_directive_monitor)
module Y = Camlp4.Register.OCamlPrinter(ID)(Make)


(*** Local Variables: ***)
(*** compile-command: "ocamlc.opt -a -o otags_dump_ast.cma -I +camlp4 conf.mli conf.ml camlp4_types.ml monitor_line_directive.ml otags_dump_ast.ml" ***)
(*** End: ***)
