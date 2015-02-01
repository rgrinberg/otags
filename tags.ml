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
 * $Id: tags.ml,v 1.29 2012/12/05 09:29:31 tews Exp $
 * 
 * recursive tagging function
 * 
 *)

open Types
open Translate_location

(* Empty asts. Used when parsing fails, to output at least the tag for
 * the module. Defined here, because tags.ml is the only file compiled
 * with quotations on. 
 *)
open Camlp4.PreCast.Syntax
let no_loc = Loc.ghost
let empty_sig_ast = Sig_ast(<:sig_item@no_loc< >>)
let empty_str_ast = Struct_ast(<:str_item@no_loc< >>)


(******************************************************************************
 *
 **********************   tagging functions   *********************************
 *
 ******************************************************************************)


let tag_constructor_decl write_tag = function
  | <:ctyp< $id:<:ident< $uid:uid$>> as id$ >> ->
    write_tag (translate_loc (Ast.loc_of_ident id)) uid
  | <:ctyp< $id:<:ident< $uid:uid$>> as id$ of $list:_$ >> ->
    (* Wrong location info in id: It covers the type expression. bug #5114
     * We need the first word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    write_tag (Reparse.loc_of_first_word wrong_id_loc) uid
  (* the next case is for GADT constructors in OCaml 4.00 *)
  | <:ctyp< $id:<:ident< $uid:uid$>> as id$ : $_$ >> ->
    (* Wrong location info in id: XXX not reported yet
     * We need the first word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    write_tag (Reparse.loc_of_first_word wrong_id_loc) uid
  | _ -> assert false


let tag_record_label_decl write_tag = function
  | <:ctyp< $id:<:ident< $lid:lid$>> as id$ : $_$ >> ->
    (* Wrong location info in id: It covers the type expression and 
     * also the keyword mutable, if present. See #5114.
     * We need the first word different from mutable.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    write_tag 
      (Reparse.loc_of_first_word_after_word "mutable" wrong_id_loc)
      lid
  | _ -> assert false



let rec tag_type write_tag typ = match typ with
  | Ast.TyDcl(_loc, id, _param_list, type_def, _constraint_list) ->
    (* missing location info for the identifier: See #5147.
     * we need the first word after the type parameters (if there are any).
     * In revised syntax it is always the first word. The heuristic is 
     * as follows:
     * - a single type parameter starts with ' or + or -
     * - multiple type parameters start with an open parenthesis
     * - in any other case the input is revised syntax or there
     *   is no type parameter
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ctyp typ) in
    (match Reparse.start_char wrong_id_loc with
      | '\'' | '+' | '-' -> 
	write_tag (Reparse.loc_of_word_after_type_param wrong_id_loc) id
      | '(' -> 
	write_tag (Reparse.loc_of_word_after_paren wrong_id_loc) id
      | 'a' .. 'z' 
      | '_' -> 
	write_tag (Reparse.loc_of_first_word wrong_id_loc) id
      | _ -> assert false
    );
    tag_type write_tag type_def
  | <:ctyp< $_t1$ == $t2$ >> -> 
    (* don't tag t1, its the abbreviation *)
    tag_type write_tag t2
  | <:ctyp< private $t$ >> -> tag_type write_tag t
  | <:ctyp< [ $t$ ] >> ->
    List.iter
      (tag_constructor_decl write_tag)
      (Ast.list_of_ctyp t [])
  | <:ctyp< { $t$ } >> -> 
    List.iter
      (tag_record_label_decl write_tag)
      (Ast.list_of_ctyp t [])

  | <:ctyp< $tup:_$ >>			(* tuple *)
  | <:ctyp< [= $_$ ] >>			(* exact variant type *)
  | <:ctyp< [> $_$ ] >>		        (* open polymorphic variant type *)
    (* closed polymorphic variant type with no known tags *)
  | <:ctyp< [< $_$ ] >>
    (* closed polymorphic variant type with some known tags *)
  | <:ctyp< [< $_$ > $_$ ] >>
    (* conjunctive type in polymorphic variants *)
  | <:ctyp< $_$ & $_$ >>
    (* [< A of & int] : impossible type in polymorphic variant types *)
  | <:ctyp< $_$ of & $_$ >>

  | <:ctyp< (module $_$) >>		(* module package type *)
  | <:ctyp< ' $_$ >>			(* type parameter *)
  | <:ctyp< < $_$ $..:_$ > >>		(* object type *)
  | <:ctyp< $id:_$ >>			(* type id *)
  | <:ctyp< # $_$ >>			(* class type *)
  | <:ctyp< $_$ -> $_$ >>
  | <:ctyp< $_$ $_$ >>			(* type constructor application *)
  | <:ctyp< $_$ as $_$ >>
  | <:ctyp< >>
    -> ()

  | <:ctyp< ! $_$ . $_$ >>              (* polymorphic type expr *)
  | <:ctyp< ? $_$ : $_$ >>		(* optional labeled function arg *)
  | <:ctyp< ~ $_$ : $_$ >>		(* optional labeled function arg *)
  | <:ctyp< $_$ * $_$ >>		(* list inside tuples *)
  | <:ctyp< $_$ | $_$ >>		(* constructor list *)
  | <:ctyp< $_$ of $_$ >>		(* constructor decl *)
  | <:ctyp< $_$ and $_$ >>		(* variant constructor argument list *)
  | <:ctyp< mutable $_$ >>		(* mutable decl in records *)
  | <:ctyp< $_$ , $_$ >>		(* class param list *)
  | <:ctyp< $_$ ; $_$ >>		(* field list in records or obj types *)
  | <:ctyp< $_$ : $_$ >>		(* record/obj field decl *)
  | <:ctyp< ` $_$ >>			(* poly variant *)
  | <:ctyp< + ' $_$ >>			(* type parameter *)
  | <:ctyp< - ' $_$ >>			(* type parameter *)
  | <:ctyp< _ >>			(* anonymous type variable *)
  | Ast.TyAnP _				(* no syntax for + _ *)
  | Ast.TyAnM _				(* no syntax for - _ *)
  | Ast.TyTypePol _			(* no syntax for type x . type-expr *)
  | <:ctyp< $anti:_$ >>
      -> assert false


let tag_class_type_decl write_tag = function
  | <:class_type< $virtual:vf$ $id:<:ident< $lid:lid$ >> as id$ [ $_$] >> ->
    (* wrong location info for id: it covers the optional virtual 
     * and the type parameters. See #5114.
     * In the original syntax we need the 
     * last word. In the revised syntax we need the first or 
     * second word, depending on whether virtual is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    (match Reparse.stop_char wrong_id_loc with
      | ']' -> (* revised syntax -- type params at end *)
	let word_number = match vf with
	  | Ast.ViVirtual -> 2
	  | _ -> 1
	in
	write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) lid
      | _ -> (* original syntax or no type parameters present *)
	write_tag (Reparse.loc_of_last_word wrong_id_loc) lid
    )
  | _ -> assert false



let rec tag_class_sig_item write_tag csi = match csi with
  | <:class_sig_item< inherit $ctyp:ct$ >> -> 
    (* The class type can be a concrete class body 
     * type object ... end, therefore we tag it.
     *)
    tag_class_type write_tag ct
  | <:class_sig_item< value $mutable:mf$ $virtual:vf$ $id$ : $_$ >> ->
    (* missing location info for id See #5147.
     * we need the second, third or fourth word, depending on wheter
     * mutable and virtual are present.
     * There is another location bug: the location of this CgVal includes
     * a following end or val. Try
     *   echo "class type a = object val b : int end" | ./dump-camlp4
     *   echo "class type a = object val b : int val c : int end" | ./dump-camlp4
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_sig_item csi) in
    let word_number = match (mf, vf) with
      | (Ast.MuNil, Ast.ViNil) -> 2
      | (Ast.MuMutable, Ast.ViVirtual) -> 4
      | _ -> 3
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id
  | <:class_sig_item< method virtual $private:pf$ $id$ : $_$ >> ->
    (* missing location info for id See #5147.
     * we need the third or fourth word, depending on wheter 
     * private is present.
     * Second location bug as for values.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_sig_item csi) in
    let word_number = match pf with
      | Ast.PrPrivate -> 4
      | _ -> 3
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id
  | <:class_sig_item< method $private:pf$ $id$ : $typ:_$ >> ->
    (* missing location info for id See #5147.
     * we need the second or third word, depending on wheter 
     * private is present.
     * Second location bug as for values.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_sig_item csi) in
    let word_number = match pf with
      | Ast.PrPrivate -> 3
      | _ -> 2
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id

  | <:class_sig_item< constraint $_$ = $_$ >>
    (* don't tag type constraints *)
    -> ()
  | <:class_sig_item< $_$ ; $_$ >>
    (* treated by list_of_class_sig_item *)
  | <:class_sig_item< >> 
    (* no empty sig items *)
  | <:class_sig_item< $anti:_$ >>
    (* no anti qotations *)
    -> assert false


and tag_class_type write_tag = function
  | <:class_type< $decl$ : $ct$ >> -> 
    tag_class_type_decl write_tag decl;
    tag_class_type write_tag ct
  | <:class_type< $decl$ = $ct$ >> ->
    tag_class_type_decl write_tag decl;
    tag_class_type write_tag ct
  | <:class_type< [ $typ:_$ ] -> $ctyp:ct$ >> -> 
    tag_class_type write_tag ct
  | <:class_type< object ($_$) $ci$ end >> -> 
    List.iter
      (tag_class_sig_item write_tag)
      (Ast.list_of_class_sig_item ci [])

  | <:class_type< $virtual:_$ $id:_$ [ $_$] >> 
    (* class id somewhere on the right hand side *)
    -> ()

  | <:class_type< $_$ and $_$ >>
    (* mutual recursive class types are filtered with list_of_class_type *)
  | <:class_type< >>
    (* there should be no empty class type *)
  | <:class_type< $anti:_$ >> 
    (* no anti quotations *)
    -> assert false


let rec tag_sig_item write_tag sig_item = match sig_item with
  | <:sig_item< exception $exid$ >> ->
    tag_constructor_decl write_tag exid
  | <:sig_item< external $id$ : $_$ = $_$ >> ->
    (* missing location info for the identifier: See #5147.
     * We need the second word for normal identifiers and the 
     * stuff inside the parens for infix identifiers.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_sig_item sig_item) in
    let loc_without_ext = Reparse.loc_without_first_word wrong_id_loc in
    let id_loc = match Reparse.start_char loc_without_ext with     
      | '(' -> Reparse.loc_inside_parens loc_without_ext
      | _ -> Reparse.loc_of_first_word loc_without_ext
    in
    write_tag id_loc id
  | <:sig_item< module $id$ : $mt$ >> -> 
    (* missing location info for the identifier: See #5147.
     * we need the second word 
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_sig_item sig_item) in
    write_tag (Reparse.loc_of_second_word wrong_id_loc) id;
    tag_module_type write_tag mt
  | <:sig_item< module type $id$ = $mt$ >> ->
    (* missing location info for the module type id: See #5147.
     * we need the third word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_sig_item sig_item) in
    write_tag (Reparse.loc_of_third_word wrong_id_loc) id;
    tag_module_type write_tag mt
  | <:sig_item< type $ts$ >> ->
    (* XXX The quotation parser for types does not parse type 
     * declarations. But there is also no general form that would match 
     * all type declarations. I therefore match type declarations 
     * in tag_type without quotations.
     *)
    List.iter (tag_type write_tag) (Ast.list_of_ctyp ts [])
  | <:sig_item< value $id$ : $_$ >> -> 
    (* missing location info for the identifier: See #5147.
     * we need the second word for normal identifiers
     * and the stuff inside the parens that follow the first word 
     * for infix identifiers.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_sig_item sig_item) in
    let loc_without_val = Reparse.loc_without_first_word wrong_id_loc in
    let id_loc = match Reparse.start_char loc_without_val with
      | '(' -> Reparse.loc_inside_parens loc_without_val
      | _ -> Reparse.loc_of_first_word loc_without_val
    in
    write_tag id_loc id
  | <:sig_item< class $cs$ >> -> 
    List.iter
      (tag_class_type write_tag)
      (Ast.list_of_class_type cs [])
  | <:sig_item< class type $cts$ >> ->
    List.iter
      (tag_class_type write_tag)
      (Ast.list_of_class_type cts [])
  | <:sig_item< module rec $mb$ >> ->
    List.iter 
      (tag_module_sig_binding write_tag)
      (Ast.list_of_module_binding mb [])

  | <:sig_item< open $_$ >> 
  | <:sig_item< include $_$ >>
    -> ()

  | <:sig_item< # $_$ $_$ >>
  | <:sig_item< $_$ ; $_$ >>
  | <:sig_item< >>
  | <:sig_item< $anti:_$ >>
    -> assert false


and tag_module_sig_binding write_tag mod_binding = match mod_binding with
  | <:module_binding< $id$ : $mtyp:mtyp$ >> ->
    (* missing location info for the identifier: See #5147
     * we need the first word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_module_binding mod_binding) in
    write_tag (Reparse.loc_of_first_word wrong_id_loc) id;
    tag_module_type write_tag mtyp

  | <:module_binding< >>
  | <:module_binding< $_$ and $_$ >> 	(* MbAnd lists are unpacked already *)
  					(* no definitions in signatures *)
  | <:module_binding< $_$ : $mtyp:_$ = $mexp:_$ >>
  | <:module_binding< $anti:_$ >>
    -> assert false


and tag_module_type write_tag = function
  | <:module_type< $id:_$ >> -> 
    (* The id here can be a long one with dots and applications.
     * However, the grammar forbits real signatures appearing inside it.
     *)
    ()
  | <:module_type< functor( $_$ : $mt1$ ) -> $mt2$ >> ->
    tag_module_type write_tag mt1;
    tag_module_type write_tag mt2
  | <:module_type< $mt$ with $_$ >> -> tag_module_type write_tag mt
  | <:module_type< sig $sig_items$ end >> -> 
    List.iter 
      (tag_sig_item write_tag)
      (Ast.list_of_sig_item sig_items [])

  | <:module_type< >>			(* emty module type ast *)
  | <:module_type< ' $_$ >>		(* ??? MtQuo ??? *)
  | <:module_type< module type of $_$ >>
    -> ()

  | <:module_type< $anti:_$ >>		(* anti quotations *)
    -> assert false


let tag_class_decl write_tag = function 
  | <:class_expr< $virtual:vf$ $id:<:ident< $lid:lid$ >> as id$ [ $_$ ] >> ->
    (* wrong location info for id: See #5114.
     * it covers the optional virtual 
     * and the type parameters. In the original syntax we need the 
     * last word. In the revised syntax we need the first or 
     * second word, depending on whether virtual is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    (match Reparse.stop_char wrong_id_loc with
      | ']' -> (* revised syntax -- type params at end *)
	let word_number = match vf with
	  | Ast.ViVirtual -> 2
	  | _ -> 1
	in
	write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) lid
      | _ -> (* original syntax or no type parameters present *)
	write_tag (Reparse.loc_of_last_word wrong_id_loc) lid
    )
  | _ -> assert false


let rec tag_class_str_item write_tag csi = match csi with
  | <:class_str_item< inherit $!:_override$ $cexp:ce$ as $lid:_$ >> ->
    (* The ce can be a class body and is therefore tagged.
     *)
    tag_class_expr write_tag ce
  | <:class_str_item< value $!:_override$ $mutable:mf$ $id$ = $_$ >> ->
    (* missing location information for id See #5147.
     * we need the second or third word, depending on whether 
     * mutable is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_str_item csi) in
    let word_number = match mf with
      | Ast.MuMutable -> 3
      | _ -> 2
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id
  | <:class_str_item< value virtual $mutable:mf$ $id$ : $_$ >> ->
    (* missing location information for id See #5147.
     * we need the third or fourth word, depending on whether 
     * mutable is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_str_item csi) in
    let word_number = match mf with
      | Ast.MuMutable -> 4
      | _ -> 3
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id
  | <:class_str_item< method virtual $private:pf$ $id$ : $_$ >> ->
    (* missing location information for id See #5147.
     * we need the third or fourth word, depending on whether 
     * private is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_str_item csi) in
    let word_number = match pf with
      | Ast.PrPrivate -> 4
      | _ -> 3
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id
  | <:class_str_item< method $!:_override$ $private:pf$ $id$ : $_$ = $_$ >> ->
    (* missing locateion information for id See #5147.
     * we need the second or third word, depending on whether 
     * private is present.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_class_str_item csi) in
    let word_number = match pf with
      | Ast.PrPrivate -> 3
      | _ -> 2
    in
    write_tag (Reparse.loc_of_nth_word word_number wrong_id_loc) id

  | <:class_str_item< constraint $_$ = $_$ >>
    (* don't tag constraints *)
  | <:class_str_item< initializer $_$ >>
    (* don't tag initializers *)
    -> ()
    
  | <:class_str_item< $_$ ; $_$ >>
    (* CrSem is filtered by list_of_class_str_item *)
  | <:class_str_item< >>
    (* should not occur *)
  | <:class_str_item< $anti:_$ >>
    (* no antiquotations *)
    -> assert false


and tag_class_expr write_tag = function
  | <:class_expr< $ci$ = $cexp:ce$ >> ->
    tag_class_decl write_tag ci;
    tag_class_expr write_tag ce
  | <:class_expr< fun $_$ -> $ce$ >> ->
    tag_class_expr write_tag ce
  | <:class_expr< let $rec:_$ $_$ in $ce$ >> ->
    tag_class_expr write_tag ce
  | <:class_expr< $ce$ $_$ >> -> 
    tag_class_expr write_tag ce
  | <:class_expr< object ( $_$ ) $cst$ end >> ->
    List.iter
      (tag_class_str_item write_tag)
      (Ast.list_of_class_str_item cst [])
  | <:class_expr< ($ce$ : $ct$) >> ->
    tag_class_expr write_tag ce;
    (* ct can contain an object body, therefore tag it *)
    tag_class_type write_tag ct

  | <:class_expr< $virtual:_$ $id:_$ [ $_$ ] >>
    (* id on the right hand side -- ignore *)
    -> ()

  | <:class_expr< $cexp:_$ and $cexp:_$ >>
    (* filtered by list_of_class_expr *)
  | <:class_expr< >> 
    (* no empty class expressions *)
  | <:class_expr< $anti:_$ >> 
    (* no antiquotations *)
    -> assert false


let rec tag_let_pattern write_tag = function
  | <:patt< $id:<:ident< $lid:lid$>> as id$ >> ->
    (* wrong location info for id if it is an infix operator. See #5114.
     * In this case the location contains the parenthesis'.
     * We need everything inside the parenthesis.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_ident id) in
    (* 
     * Printf.eprintf "TAGPATT %s %s -> %s\n%!" 
     *   lid
     *   (Source_channel.full_string_of_loc (Ast.loc_of_ident id))
     *   (Source_channel.full_string_of_loc wrong_id_loc);
     *)
    let loc =
      match Reparse.start_char wrong_id_loc with
	| '(' -> Reparse.loc_inside_parens wrong_id_loc
	| _ -> wrong_id_loc
    in
    write_tag loc lid;
    (* Printf.eprintf "TAGPATTEND\n%!" *)

  (* let matching with constant constructors: let A = ... in ... *)
  | <:patt< $uid:_$ >>
  | <:patt< $id: <:ident< $_$ . $_$ >> $ >>
  | <:patt< $id: <:ident< $anti:_$ >> $ >> -> ()
  | <:patt< $id: <:ident< $_$ $_$ >> $ >> -> assert false

  | <:patt< ( $tup:p1$ ) >> ->
    List.iter
      (tag_let_pattern write_tag)
      (Ast.list_of_patt p1 [])
  | <:patt< ($p1$ as $p2$) >> ->
    (* In standard ocaml p2 must be an identifier, however, the 
     * revised syntax only requires that one of p1 and p2 is 
     * an identifier. This is important because for standard ocaml the 
     * location for p2 is incorrect, see #5114, but the correction that 
     * we apply does only work if p2 is an identifier.
     * When the location bug is fixed one can simply do
     *    tag_let_pattern write_tag p1;
     *    tag_let_pattern write_tag p2
     *)
    (match (p1, p2) with
      | (_, <:patt< $lid:lid$ >>) -> 
	(* Assume normal parser *)
	tag_let_pattern write_tag p1;
	(* wrong location info for p2:
	 * we need the last word
	 *)
	let wrong_id_loc = translate_loc (Ast.loc_of_patt p2) in
	write_tag (Reparse.loc_of_last_word wrong_id_loc) lid

      | _ -> 
        (* Assume revised parser --- which gives correct locations *)
	tag_let_pattern write_tag p1;
	tag_let_pattern write_tag p2
    )
  | <:patt< ($p1$ : $_$) >> ->
    tag_let_pattern write_tag p1

  | <:patt< $chr:_$ >>
  | <:patt< $int:_$ >>
  | <:patt< $int32:_$ >>
  | <:patt< $int64:_$ >>
  | <:patt< $nativeint:_$ >>
  | <:patt< $str:_$ >>
  | <:patt< $flo:_$ >>
  | <:patt< [| $_$ |] >>
  | <:patt< { $_$ } >>
  | <:patt< ` $_$ >>
  | <:patt< # $_$ >>
  | <:patt< $_$ .. $_$ >>
  | <:patt< $_$ | $_$ >>
  | <:patt< $_$ $_$ >>			(* eg, constructor application :: *)
  | <:patt< _ >>
  | <:patt< lazy $_$ >>
  | <:patt< $anti:_$ >>
    -> ()

  | <:patt< $_$ = $_$ >> 		(* only inside records *)
  | <:patt< ~ $_$ : $_$ >>	        (* only inside fun expressions *)
  | <:patt< ? $_$ : ( $_$ ) >>	        (* only inside fun expressions *)
  | <:patt< ? $_$ : ( $_$ = $_$ ) >>	(* only inside fun expressions *)
  | <:patt< $_$ ; $_$ >>		(* only inside arrays, records *)
  | <:patt< $_$ , $_$ >>		(* handled by Ast.list_of_patt *)
  | <:patt< ( module $_$ ) >>  (* package pattern not permitted in top-level *)
  | <:patt< >> 
    -> assert false



let rec tag_str_item write_tag str_item = match str_item with
  | <:str_item< exception $exid$ >> 
  | <:str_item< exception $exid$ = $_$ >> ->
    tag_constructor_decl write_tag exid
  | <:str_item< external $id$ : $_$ = $_$ >> ->
    (* missing location info for the identifier: See #5147.
     * We need the second word for normal identifiers and the 
     * stuff inside the parens for infix identifiers.
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_str_item str_item) in
    let loc_without_ext = Reparse.loc_without_first_word wrong_id_loc in
    let id_loc = match Reparse.start_char loc_without_ext with     
      | '(' -> Reparse.loc_inside_parens loc_without_ext
      | _ -> Reparse.loc_of_first_word loc_without_ext
    in
    write_tag id_loc id
  | <:str_item< module $uid$ = $me$ >> ->
    (* missing location info for the module id: See #5147.
     * we need the second word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_str_item str_item) in
    write_tag (Reparse.loc_of_second_word wrong_id_loc) uid;
    tag_module_expr write_tag me
  | <:str_item< module type $id$ = $mt$ >> ->
    (* missing location info for the module type id: See #5147.
     * we need the third word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_str_item str_item) in
    write_tag (Reparse.loc_of_third_word wrong_id_loc) id;
    tag_module_type write_tag mt
  | <:str_item< type $ts$ >> ->
    (* XXX The quotation parser for types does not parse type 
     * declarations. But there is also no general form that would match 
     * all type declarations. I therefore match type declarations 
     * in tag_type without quotations.
     *)
    List.iter (tag_type write_tag) (Ast.list_of_ctyp ts [])
  | <:str_item< value $rec:_$ $binding$ >> ->
    (* 
     * Printf.eprintf "TAGLET %s\n%!" 
     *   (Loc.to_string (Ast.loc_of_str_item str_item));
     *)
    List.iter 
      (fun (p, _expr) -> tag_let_pattern write_tag p)
      (Ast.pel_of_binding binding);
    (* Printf.eprintf "TAGLETEND\n%!" *)
  | <:str_item< class $cdcl:cs$ >> ->
    List.iter
      (tag_class_expr write_tag)
      (Ast.list_of_class_expr cs [])
  | <:str_item< class type $cts$ >> ->
    List.iter
      (tag_class_type write_tag)
      (Ast.list_of_class_type cts [])
  | <:str_item< module rec $mb$ >> ->
    List.iter
      (tag_module_def_binding write_tag)
      (Ast.list_of_module_binding mb [])


  | <:str_item< open $_$ >>
  | <:str_item< include $_$ >>
  | <:str_item< $exp:_$ >>
  | <:str_item< # $_$ $_$ >>		(* directive *)
    -> ()

    (* XXX there is no quotation syntax for the following *)
  | Ast.StExc(_, _, Ast.OAnt _)
  | <:str_item< >>
  | <:str_item< $_$ ; $_$ >> (* StSem treated in the caller *)
  | <:str_item< $anti:_$ >> 
    -> assert false


and tag_module_def_binding write_tag mod_binding = match mod_binding with
  | <:module_binding< $id$ : $mtyp:mtyp$ = $mexp:mexp$ >> ->
    (* missing location info for the identifier: See #5147
     * we need the first word
     *)
    let wrong_id_loc = translate_loc (Ast.loc_of_module_binding mod_binding) in
    write_tag (Reparse.loc_of_first_word wrong_id_loc) id;
    tag_module_type write_tag mtyp;
    tag_module_expr write_tag mexp

  | <:module_binding< >>
  | <:module_binding< $_$ and $_$ >> 	(* MbAnd lists are unpacked already *)
  					(* no specifications in structures *)
  | <:module_binding< $_$ : $mtyp:_$ >>
  | <:module_binding< $anti:_$ >>
    -> assert false


and tag_module_expr write_tag = function 
  | <:module_expr< functor ( $_$ : $_$ ) -> $me$ >> ->
    (* XXX tag arg signature? *)
    tag_module_expr write_tag me
  | <:module_expr< ( $me$ : $_$ ) >> ->
    tag_module_expr write_tag me
  | <:module_expr< struct $str_items$ end >> ->
    List.iter
      (tag_str_item write_tag)
      (Ast.list_of_str_item str_items [])

  | <:module_expr< $_$ $_$ >>		(* XXX check and probably tag this *)
  | <:module_expr< $id:_$ >>
  | <:module_expr< (value $exp:_$) >>	(* module expressions *)
    -> ()

  | <:module_expr< $anti:_$ >>
  | <:module_expr< >>
    -> assert false


(* Generate tags for unit_ast of file file by calling write_tag 
 * for each tag.
 *)
let generate_tags write_tag unit_ast =
  (match unit_ast with
    | Sig_ast sig_unit ->
      List.iter
	(tag_sig_item write_tag)
	(Ast.list_of_sig_item sig_unit [])
    | Struct_ast str_unit ->
      List.iter
	(tag_str_item write_tag)
	(Ast.list_of_str_item str_unit [])
  )

