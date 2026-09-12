# RakuAST: an absent optional field answers instead of dying

A RakuAST node stores only the fields its source actually produced. The renderer
elides an `if` with no `else`, an empty `elsifs`, a `dwim-left` that is false —
and until now so did the accessor layer, which built a node's method set out of
the fields the node happened to hold. Asking a statement for a modifier it did
not have was therefore a hard error:

```
$ mutsu -e 'say Q[say 1 with 2].AST.statements[0].loop-modifier.defined'
No such method 'loop-modifier' for invocant of type 'RakuAST::Statement::Expression'
```

Rakudo declares every field as an attribute, so the same call is legal there and
answers with an undefined value — which is exactly what makes `.defined` the
natural way to test for an optional clause:

```
$ raku -e 'say Q[say 1 with 2].AST.statements[0].loop-modifier.defined'
False
```

The cost of the gap was paid in the tests: "the `with` modifier does not occupy
the loop-modifier slot" had to be spelled `unlike ....AST.gist, /'loop-modifier'/`,
a string match standing in for a structural check, and the same trick pinned
"a `with` block inside an `else` is a statement, not an `elsif` clause"
(#8123's release note flagged both).

## What changed

The declaration is now separate from what a node carries.
`src/rakuast/fields.rs` holds, per class, the fields that class declares
together with what each answers when the node does not carry it, and
`node_accessor` consults it after its lookup over the node's own fields misses.
`.^attributes(:local)` and `.^methods(:local)` are derived from the same table,
so introspection and dispatch can no longer disagree about which accessors
exist.

Each answer is a measured shape, not a uniform `Nil`:

- an absent node-typed field answers with its **declared type object** —
  `.else` on an `if` without one is `(Block)`, `.loop-modifier` is `(Loop)`,
  `.initializer` on `my $x` is `(Initializer)`;
- an absent `List` field answers `()`, never undefined — an `if` with no `elsif`
  has `.elsifs.elems == 0`, not an exception and not a type object;
- a `Bool` flag the renderer elides answers `False`, and an `int` flag `0`
  (`Pragma.off`);
- a field whose *empty* value the renderer elides answers with that empty node:
  `sub f { }` gists with no `signature`, but `.signature` is a defined,
  parameterless `RakuAST::Signature`, as on rakudo;
- `Parameter.optional` is tri-state on rakudo — `False` on a plain positional,
  but left unset when optionality follows from something else (a default, a
  slurpy, a named parameter) — so an absent one answers with an undefined
  `Bool`;
- a structurally required field (`ApplyInfix.left`) declares no absent answer and
  still falls through to ordinary "no such method" dispatch, rather than
  inventing a value for a malformed node.

Two ad-hoc special cases disappeared into the table (`MetaInfix::Hyper`'s
`dwim-left`/`dwim-right`, and `Pragma`'s `argument`/`off`), and both were
answering with the wrong *type* while being right about truthiness:
`Pragma.argument` handed back `Nil` where rakudo has `(Expression)`, and
`Pragma.off` `False` where rakudo has `0`.

Alongside this, `RakuAST::StatementModifier::If` / `::Unless` / `::With` /
`::Without` / `::Given` gained the `.expression` accessor they had never
exposed, and `Statement::Expression` gained the `condition-modifier` slot it
could fill but not declare.

## Verification

A sweep over 21 sample programs walked every node of each tree and called every
declared accessor under both implementations: 289 lines of class/accessor/
type/definedness, identical but for one line. That one is a pre-existing
converter difference unrelated to this change — `Parameter.slurpy` stores an
empty node in mutsu where rakudo stores the `RakuAST::Parameter::Slurpy::*`
type object itself, so the field renders identically but reads as defined
(filed as #8157).

`t/rakuast/rakuast-absent-field-accessors.t` pins the mechanism, and the two
`.gist` string matches named above are now the structural checks they were
standing in for. Every assertion passes under both mutsu and raku.

Closes #8124.
