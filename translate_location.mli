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
 * $Id: translate_location.mli,v 1.2 2012/02/17 13:06:25 tews Exp $
 * 
 * translate locations after line directives
 * 
 *)

open Types

val prepare_line_directives : unit -> unit

val translate_loc : loc_t -> loc_t
