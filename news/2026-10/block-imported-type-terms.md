# A block's imported types are terms of its pad

A block-scoped `use` used to bind an imported type through an env alias. The
alias left with the block's dynamic extent, so a method of a class declared
in the block, run after the block exited, fell back to the program's own type
of the same name (`do { use M; class U { method make { Foo.new } } }`).

Now a type the parse-time scan knows `M` exports is bound like an exported
constant (ADR-8751): the compiler binds it as `my \Foo := M::Foo` right after
the `use`, and the import writes no env alias for it. The block's parents and
roles (`class C is Foo`, `does Foo`) are qualified at parse time, as its type
positions already were. A routine written outside the block still does not
see the import. The names the block binds are hidden from it on every run of
the `use`, the replayed ones included.

Two fixes came with this:

- A method's declaration-time capture dropped a sigilless binding of the
  scope that declares its class. `do { my \T = 1; class K { method t { T } };
  K.new }.t` read `(Any)`.
- #14068 is filed for the same gap with a `my constant`.
