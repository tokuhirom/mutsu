# `gist`, `raku` and `WHICH` on the scalar types are method-table rows

`Int`, `Num`, `Rat`, `Complex`, `Bool` and `Str` now register their `gist`, `raku` and `WHICH` (and `Bool.Str`) as
rows of the one method table (ADR-11276 §9.30). `.WHICH` is a single function shared by the table and the cascade,
and the cascade's `Bool`, `Rat` and `Str` rendering arms call the same handlers as the rows. The collections, the
objects group and `clone`/`fmt` are the next steps of the rendering and identity names.
