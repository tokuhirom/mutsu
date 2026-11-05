# A class range between two surrogate code points is empty, not an error

`<[ \x[D800] .. \x[DBFF] ]>` (Java's UTF-16 surrogate ranges in ANTLR-generated grammars) is accepted and matches nothing, as in rakudo, in a regex literal and in a grammar token; a genuinely reversed range is still refused. ANTLR4::Grammar `t/03-corpus-compile.t` passes 48/48.
