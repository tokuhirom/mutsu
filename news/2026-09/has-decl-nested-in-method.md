# `has` declares correctly when nested inside a method, and tolerates a trailing compound-assign

Rakudo treats `has` as a compile-time declarator: the attribute it names is
installed on the enclosing class wherever the `has` textually sits, even
nested inside a method body or a control-flow block within one, and even if
that code path never actually runs. mutsu already special-cased this for a
`has` nested directly inside a top-level `sub` in a class body; it did not
extend to a `method`, nor to any control-flow nesting, nor did the runtime
path handle the statement being reached a second time.

Two related gaps, both surfaced by `PDF::Font::Loader::Enc::CMap`'s
`if $!next-cid >= ... { has $!out-of-gas //= warn "..." }`:

- **Attribute discovery** (`class_own_attribute_names`/`compile_class_attr_decls`)
  only recursed into a `sub`'s own direct statement list. It now shares one
  general walker (`collect_nested_has_decl_stmts`, `opcode.rs`) that also
  descends into `method`/`submethod` bodies and into `if`/`while`/`for`/
  `loop`/`given`/`when`/etc. nested within either, at arbitrary depth,
  stopping only at a nested `class`/`role` (which owns its own attribute
  scope).
- **Runtime re-entry**: reaching the `has` statement when the method actually
  runs used to always throw `X::Attribute::NoPackage` (the opcode only knew
  how to register an attribute while a class was *actively being defined* via
  `BEGIN`/`EVAL`). The compiled op now also carries the class it was lexically
  compiled under and treats reaching an already-installed attribute as a
  no-op — matching rakudo, where the declaration's runtime control-flow
  position has no effect.

Separately, `has $decl` followed by a non-initializer infix (`has $!g //=
EXPR`, as in the CMap snippet) used to fail to parse: rakudo's declarators
only recognize `=`, `:=`, `::=`, `.=` as initializers, so `//=` leaves the
declaration to be parsed as a bare term with the operator applying
afterward — mutsu's `has_decl` was statement-only and had nowhere to put a
trailing infix. It now recognizes a compound-assignment operator here and
splices the statement into a plain declaration followed by an ordinary
compound assignment against the declared variable (for all three attribute
forms: `$!priv`, `$.pub`, and the bare alias). This is a deliberate, narrower
stand-in for rakudo's actual semantics, where the declaration used as a term
evaluates to a symbolic constant (the attribute's type object) rather than a
live reference — so `//=`/`||=` against it always dies with "Cannot modify an
immutable Nil value" in real rakudo, regardless of twigil. Reproducing that
exactly needs the "declarator usable in arbitrary term position" architecture
change the tracking issue calls out as separate and large; mutsu's variable
stays real and mutable instead, which is enough to let the construct compile
and run usefully (and is never exercised either way by the distribution's own
test suite).

Pinned by `t/oo/method/has-decl-nested-in-method.t`.

Closes #8441.
