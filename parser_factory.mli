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
 * $Id: parser_factory.mli,v 1.6 2012/05/21 09:29:29 tews Exp $
 * 
 * build new camlp4 parsers
 * 
 *)

open Types

type parser_functions = { 
  mkloc : string -> loc_t;
  parse_implem : 
    ?directive_handler:(str_item_t -> str_item_t option) -> 
		                       loc_t -> char Stream.t -> str_item_t;
  parse_interf : 
    ?directive_handler:(sig_item_t -> sig_item_t option) -> 
		                       loc_t -> char Stream.t -> sig_item_t;
}


val update_syntax : string list -> parser_functions
