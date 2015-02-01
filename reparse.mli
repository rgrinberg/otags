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
 * $Id: reparse.mli,v 1.8 2012/01/14 21:31:39 tews Exp $
 * 
 * location-parsing hack for those ast nodes that do not provide
 * sufficient location info
 *)

open Types

(* Return the character at the start position.
 *)
val start_char : loc_t -> char


(* Return the end character, that is, the character before
 * the end position
 *)
val stop_char : loc_t -> char


(* computes the location of the n-th word in the string described 
 * by loc
 *)
val loc_of_nth_word : int -> loc_t -> loc_t


(* same as loc_of_nth_word 1
 *)
val loc_of_first_word : loc_t -> loc_t


(* same as loc_of_nth_word 2
 *)
val loc_of_second_word : loc_t -> loc_t


(* same as loc_of_nth_word 3
 *)
val loc_of_third_word : loc_t -> loc_t


(* returns location without the first word and the following white space 
 *)
val loc_without_first_word : loc_t -> loc_t


(* returns the location of the word after skipping over one 
 * type parameter. The start position must be the start position 
 * of the type parameter.
 *)
val loc_of_word_after_type_param : loc_t -> loc_t


(* return the location of the word after the first closing parenthesis ')'
 *)
val loc_of_word_after_paren : loc_t -> loc_t


(* loc_of_first_word_after_word word loc
 * returns the location of the first word in the string designated by loc,
 * except for the case when this string starts with word, then the 
 * location of the second word is returned.
 *)
val loc_of_first_word_after_word : string -> loc_t -> loc_t

(* loc_of_last_word loc 
 * computes the location of the last word in the string described 
 * by loc in channel ic
 *)
val loc_of_last_word : loc_t -> loc_t


(* chop parenthesis on start and end and skip the surrounding 
 * white space.
 *) 
val loc_inside_parens : loc_t -> loc_t
