# A hash in a scalar answers `[0]:exists` as a key lookup, not positionally

`Hash` does `Associative` but not `Positional`, so raku reads a positional
subscript of one through `Any.AT-POS`/`Any.EXISTS-POS` — the hash is a
one-element list holding itself:

```raku
my $c = { a => 1 };
say $c[0]:exists;   # raku: True,  mutsu: False
say $c<a>:exists;   # raku: True,  mutsu: True (agrees)
```

The one-element-list rule was implemented for plain scalars in
[the scalar `:exists` slice](../../news/2026-07/scalar-positional-exists.md), and
`Value::is_one_element_scalar` deliberately excludes `Hash` for the reason this
ticket exists: the exists opcode does not carry the subscript kind, so it cannot
tell `$c[0]` from `$c{0}`. Applying the positional rule to a hash target would
break the numeric-key lookup, which mutsu currently gets right:

```raku
my %h = 0 => "x";
say %h{0}:exists;   # raku: True,  mutsu: True — must stay True
```

So this needs the `[ ]`-vs-`{ }` distinction to reach the opcode (a flag in the
`ExistsIndex*` opcode operands, set by the parser from the bracket it saw),
which is a compiler + opcode change rather than a runtime predicate tweak. The
same missing distinction is what forces
`todo/tickets/scalar-subscript-value-adverbs.md` to invent an index-shape
heuristic, and what the delete opcode already works around by picking the
protocol method from the index's type — so the three would be settled together
by carrying the subscript kind.

`Set`/`Bag`/`Mix` in a scalar have the same shape (raku: `(my $s = <a b>.Set)[0]:exists`
is True) and the same obstacle.
