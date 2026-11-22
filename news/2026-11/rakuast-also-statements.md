# RakuAST: `also is rw;` is a `Statement::Also`; `also` statements are placed

`class C { also is rw; }` was viewed with `Trait::Is(rw)` among the class's
own traits and no statement in the body. Rakudo 2026.11 keeps a
`Statement::Also` holding the trait in the body. The parser lifts `also is rw`
into the class's `rw` flag as before, and now also records that it was
written so (`PackageMarks::rw_also`). The view puts the statement back, and
the lowering turns it into the statement the parser reads it as, which the
class lowering lifts again. `EVAL` of the tree keeps the class `rw`.

The `also` statements of a package body (`also is Int;`, `also does R;`,
`also is rw;`) are view-made and had no ranges. Their trait's `Trait::Is` /
`Trait::Does` and its name or type were unplaced too, even where the
statement had one. They are now found in the body's text in source order and
placed (`also_origins`), as Rakudo places them. This closes #18598, whose
other three shapes landed earlier.
