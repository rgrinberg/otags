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
 * $Id: monitor_line_directive.ml,v 1.1 2012/02/17 13:06:25 tews Exp $
 * 
 * filter and store line directives
 * 
 *)

type line_directive_record = {
  ld_file : string;
  ld_line : int;
  ld_parse_offset : int;
  mutable ld_file_offset : int;
}

let parsed_line_directives = ref []

let parse_line_directive file line parse_offset =
  parsed_line_directives := 
    { ld_file = file;
      ld_line = line;
      ld_parse_offset = parse_offset;
      ld_file_offset = -1
    } :: !parsed_line_directives


module Sig = Camlp4.Sig

module Line_directive_monitor(Syn : Sig.Camlp4Syntax) : Sig.Camlp4Syntax =
struct 
  include Syn

  (* 
   * type token_filter = (t, Loc.t) stream_filter
   * type token_filter = (t * Loc.t) Stream.t -> (t * Loc.t) Stream.t
   *)

  let monitor_line_directive token loc = match token with
    | Sig.LINE_DIRECTIVE(line, file_opt) ->
      (* 
       * Printf.eprintf "LD [%s: sl %d(%d) so %d el %d(%d) eo %d%s] : %d %s\n%!"
       * 	(Loc.file_name loc)
       * 	(Loc.start_line loc) (Loc.start_bol loc) (Loc.start_off loc)
       * 	(Loc.stop_line loc) (Loc.stop_bol loc) (Loc.stop_off loc)
       * 	(if Loc.is_ghost loc then " GHOST" else " REAL")
       * 	line
       * 	(match file_opt with
       * 	  | Some f -> f
       * 	  | None -> "<no file>"
       * 	);
       *)
      (match file_opt with
	| None -> assert false
	| Some file -> parse_line_directive file line (Loc.stop_off loc)
      )

    | Sig.KEYWORD       _
    | Sig.SYMBOL        _
    | Sig.LIDENT        _
    | Sig.UIDENT        _
    | Sig.ESCAPED_IDENT _
    | Sig.INT           _
    | Sig.INT32         _
    | Sig.INT64         _
    | Sig.NATIVEINT     _
    | Sig.FLOAT         _
    | Sig.CHAR          _
    | Sig.STRING        _
    | Sig.LABEL         _
    | Sig.OPTLABEL      _
    | Sig.QUOTATION     _
    | Sig.ANTIQUOT      _
    | Sig.COMMENT       _
    | Sig.BLANKS        _
    | Sig.NEWLINE
    | Sig.EOI
	-> ()

  let monitor_map_stream stream =
    let next _ = match Stream.peek stream with
      | Some (token, loc) as peek_res -> 
	Stream.junk stream;
	monitor_line_directive token loc;
	peek_res
      | None -> None
    in
    Stream.from next

  let _ =
    Gram.Token.Filter.define_filter 
      (Gram.get_filter ()) 
      (fun f s -> f (monitor_map_stream s))
end

