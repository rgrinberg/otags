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
 * $Id: tags.mli,v 1.9 2012/01/14 21:31:40 tews Exp $
 * 
 * recursive tagging function
 * 
 *)

open Types

(* Empty ast of an interface. Used when parsing fails to output 
 * at least the tag for the module. Defined here, because tags.ml is
 * the only file compiled with quotations on.
 *)
val empty_sig_ast : comp_ast


(* Empty ast of an implementation unit. Used when parsing fails, see 
 * empty_sig_ast.
 *)
val empty_str_ast : comp_ast


(* generate_tags write_tag unit_ast file
 * generates tags for unit_ast of file file by calling write_tag 
 * for each tag.
 *)
val generate_tags : write_tag_t -> comp_ast -> unit
