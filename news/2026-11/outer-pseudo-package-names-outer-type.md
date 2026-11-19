# `OUTER::` names the type the reached scope declares

```raku
class Inner { method tag { "outer" } }
sub b() { { my class Inner { method tag { "block" } }; say OUTER::OUTER::Inner.tag } }
b();   # was block, now outer as in Rakudo
```

`OUTER::` names exactly one enclosing scope. For variables the compiler
already settled that lexically, but a type name (`OUTER::OUTER::Inner`,
`OUTER::OUTER::<Inner>`) was looked up at run time by its spelling. That
answered whichever same-named type the running scope saw, here the block's
`my class`. Each scope now records the `class` and `role` declarations it makes,
under the name the registry holds them by. An `OUTER::` lookup of a type that
the reached scope declares compiles to that type object, so a nearer
same-named `my class` no longer shadows it (#18213).
