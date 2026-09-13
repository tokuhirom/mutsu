# Parse-failure index: stacked contextualizers, statement-prefix pair keys, angle-word subscripts, wrapped custom infixes

Four more constructs off the `expected statement ...` parse-failure index
([#7954](https://github.com/tokuhirom/mutsu/issues/7954)). As the previous batch's
handover advised, the targets were **re-derived** from the current
`ecosystem/dists/` records rather than taken from the issue's snapshot table —
and then each candidate was re-checked against rakudo on the same file, which
turned out to matter more than expected (see "How the targets were chosen").

## A contextualizer takes a circumfix, and contextualizers stack

`TOML`, `Clu`. `lib/TOML/NQP.rakumod` has a line of dead code left after a
`return`:

```raku
              &&($pos >= nqp::chars($t)
              || nqp::ordat($t, $pos) ~~ (0x20|0xA|0xC));
```

`&( ... )` is the callable contextualizer, and its operand is a *circumfix* — a
whole parenthesized group, so it may hold a comma list or a semilist. mutsu
parsed only a single expression inside `&(`, which stopped at the first comma
and failed at the `)`. It also had no branch at all for a second `&`, so a
statement opening with `&&(` was not a term in any reading.

Both follow from rakudo's own grammar, where the contextualizer's operand is
`<circumfix>` and a contextualizer is itself a term: `&&(0, 1)` is `&(&(0, 1))`
and `&&f` is `&(&f)`. `code_var` now delegates the parenthesized form to
`paren_expr` and recurses on a leading `&`. The single-operand forms are
unchanged, and `&&` in infix position is still the logical-and operator, since
that position never reaches a term parser.

## A bareword followed by `=>` is a pair key — including four that were not

`Qwiratry`. `%(lazy => %(enabled => $enabled, type => $type))` did not parse:
"Preceding context expects a term, but found infix => instead".

The cause was not the nesting but the key. `lazy`, `eager`, `hyper` and `race`
are statement prefixes matched in `prefix_expr`, which runs ahead of
`identifier_call` — and `identifier_call` is where the "an identifier followed
by `=>` is a pair key regardless" rule lives, the rule that already lets
`(my => 1)` and `(class => 1)` be pairs. Those four words were therefore the
only ones in the language that could not name a pair anywhere except inside a
`{ ... }` hash composer: `(lazy => 1)`, `[eager => 1]`, `%(race => 1)` and even
the named argument `f(hyper => 1)` were all hard parse errors.

The four branches now consult the same `next_is_bareword_fat_arrow_pair`
predicate before claiming the word. Their statement-prefix meanings are
untouched — only a following `=>` diverts them.

## An angle subscript is a word quote, and validates nothing

`BigRoot`. `state %results = Hash<RootNumber, FatRat, Natural>.new;`.

`<...>` is a Q-style word quote: rakudo splits it on whitespace and every other
character is an ordinary member of a word. So `%h<a, b>` is a two-key slice
whose keys carry the commas, and `%h<a|b>` is the single key `a|b`. mutsu
instead validated each key against an allowlist of "key characters" whose
doc comment already stated the real rule — and which had been extended one
character at a time, `=` then `(` `)` then `#`, each for one distribution that
tripped over it. The comma was next in that series.

The allowlist is now two predicates with distinct jobs. The *subscript* path
uses `is_angle_subscript_key_char`, which implements the documented rule (any
non-whitespace character other than `>`, plus a non-breaking space, which is
not a word separator). The lvalue-versus-comparison lookahead in `try_assign`
keeps the conservative set under the name `is_conservative_angle_key_char`,
because being permissive *there* would claim source that is not a subscript at
all.

## A declared custom infix continues an expression across a newline

`Arithmetic::PaperAndPencil`. A long expression wrapped after its first operand:

```raku
  return Arithmetic::PaperAndPencil::Number.new(:radix($radix), :value($s))
      ☈+ Arithmetic::PaperAndPencil::Number.new(:radix($radix), :value<1>);
```

A built-in symbolic infix on a continuation line already worked; a user-declared
one died with "Undeclared routine", because the statement ended at the first
operand and the operator opened a new one.

The guard that caused it is real but was applied too widely. `parse_custom_infix_word`
is deliberately speculative — it matches *any* non-reserved bareword, so that an
operator installed at runtime (`my &infix:<...> = ...`) still parses — and a
speculative match across a newline would swallow the next statement whole. An
operator the parser has actually seen declared carries no such ambiguity: it is
in the operator table by then, and the newline before it is ordinary whitespace.
Both call sites now let the newline through for exactly that case, via
`is_declared_custom_infix_word`.

## How the targets were chosen

The re-derivation was: for every `blocked_load` record whose `load` map holds a
parse error, fetch the tarball and `--dump-ast` each module the META6
`provides` names. That gives a current list, since the sweep behind the issue's
table ran at `36527ea`.

**It also produces false positives, and the largest apparent cluster was one.**
Seven distributions failed with "Function 'X::Foo::Bar' needs parens to avoid
gobbling block", every one of them at a `when X::Something { ... }` — which
looks like a missing feature until you run rakudo on the same single file and
get *the same error, verbatim*. The exception class comes from a `use`d module,
`--dump-ast` resolves no imports, and rakudo does not either when the dependency
is absent. So a per-file dump must be paired with a rakudo control run on the
same file before anything in it counts as a divergence; without that control,
this batch would have spent itself implementing behaviour mutsu already has.

Of the 88 distributions checked, `BigRoot` and `TOML` now parse every module
their META6 names, `Clu` unblocks with `TOML::NQP`, and `Qwiratry` and
`Arithmetic::PaperAndPencil` each lose a failing module.

## Pins

`t/lang/sigil-contextualizer-circumfix.t`,
`t/lang/operators/fat-arrow-statement-prefix-key.t`,
`t/collections/subscript/angle-subscript-word-chars.t`,
`t/lang/operators/user-infix-across-newline.t` — all four green under rakudo
itself, so they pin rakudo's behaviour rather than mutsu's.
