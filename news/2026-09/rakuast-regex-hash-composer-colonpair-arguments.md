# Hash-composer regex colonpairs preserve their Hash value

Argumented regex subrules now retain hash-composer block-valued colonpairs in
RakuAST, including multiple `FatArrow` entries such as
`:expected{ a => $value, b => 2 }`. Constructed regex trees lower them back
through the existing parser/compiler/VM matcher, preserving named Hash binding
and match-time lexical reassignment.

Complex or unrenderable block values remain explicit boundaries.
