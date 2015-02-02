
Otags Reloaded: TAGS generation for emacs and vi from OCaml sources
===========================================================================

Installation
============

```
$ opam pin add otags https://github.com/rgrinberg/otags
```

Summary
=======

Otags can be used to build TAGS tables for emacs and vi, like etags
does, but for OCaml code source files. It is based on the camlp4
parsers, which makes it more accurate than versions based on regexps:
otags finds references to constructors of sum types, fields of
records, etc. The downside of the camlp4 approach is that you can only
tag syntactically correct files and that otags only accepts what
camlp4 thinks is correct (which is sometimes different from ocamlc).

USAGE
=====

To build an emacs TAGS table for ocaml.ml in standard syntax and
for quotation.ml in standard syntax with revised quotations do:

```
$ otags ocaml.ml -pa rq quotation.ml
```

(Note that otags switches the parsing engine between the two
files.)

Most important options are `-r` for recursive directory tagging and
`-vi` for generating vi/vim tags tables.

For more examples and all details visit the man page in the doc
subdirectory or online at http://askra.de/otags/otags.man.html

CREDITS / MISC
==============

The first camlp4 based OCaml tagger has been written by
Cuihtlauac Alvarado (http://perso.rd.francetelecom.fr/alvarado)
and Jean-Francois MONIN (http://www-verimag.imag.fr/~monin).
Around 2005 I became maintainer. All otags versions released up
to 3.09.3.3 were written for the original camlp4, which is now
available as camlp5. 

This version has been completely rewritten for the new camlp4. Its
most distinct new features are that it can tag files in different
syntax at once and that files with arbitrary combinations of the
standard Camlp4 syntax extensions are parsed with full native speed
without exernal Camlp4 processes.


Contact
=======

Please send bug reports, comments, patches, donations to 
Hendrik Tews <otags@askra.de>
