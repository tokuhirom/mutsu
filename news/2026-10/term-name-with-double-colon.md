# A `::` inside a categorical name is part of the name, not a package qualifier

`sub term:<Foo::Bar> () { 1 }` could be called at the top level but not from inside a sub,
closure or method (`Could not find symbol '&Bar>' in 'GLOBAL::term:<Foo'`), because the
layers that take a name apart at `::` each split it at the `::` inside the angle brackets.
The ecosystem's `IP::Addr` declares `sub term:<IP::Addr>` and was blocked on it.

`qualified::separators` now defines the package separators of a name once: a `::` inside
the `<…>` / `«…»` group of a categorical (`term:<…>`, `infix:sym<…>`) belongs to the name,
while a package in front of the categorical (`Pkg::term:<In::Pkg>`) still qualifies it.
`is_qualified`, `split_qualified`, `segments`, `package_parent`, the package-variable
classification and the function-key base-name scan (`function_key_base_name`) all use it,
so they cannot disagree (#11822).
