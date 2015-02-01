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
 * $Id: add_quotation.ml,v 1.4 2012/01/14 21:31:38 tews Exp $
 * 
 * higher-order functor for adding quotations to some syntax module
 * 
 *)

module PreCast = Camlp4.PreCast
module type Camlp4Syntax = Camlp4.Sig.Camlp4Syntax 
  with module Loc = PreCast.Loc
  and module Ast = PreCast.Ast

module Make
  (HostSyntax : Camlp4Syntax)
  (MakeQuotationSyntax : functor(EmptySyntax : Camlp4Syntax) -> Camlp4Syntax) 
  =
struct
  module Gram = PreCast.MakeGram(PreCast.Lexer)
  module EmptySyntax = 
    Camlp4.OCamlInitSyntax.Make(PreCast.Ast)(Gram)(HostSyntax.Quotation)
  module QuotationSyntax = MakeQuotationSyntax(EmptySyntax)
  module X = 
    Camlp4QuotationCommon.Make(QuotationSyntax)(HostSyntax.AntiquotSyntax)
end
