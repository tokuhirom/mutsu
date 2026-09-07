# A `Block` inside a list renders as nothing, so a non-empty list prints as `()`

`say` of a list whose element is a `Block` prints an **empty list**. The element
is there — `.elems` is 1 — but it renders as the empty string, so the output is
indistinguishable from an empty list.

## Repro

```
my &b = { $^a }; say (&b,);        # mutsu ()      raku (-> $a { #`(Block|...) ... })
my &b = { $^a }; say [&b];        # mutsu []      raku [-> $a { #`(Block|...) ... }]
my &b = { $^a }; say (&b,).elems; # 1 in BOTH  <- the element exists
my &b = { $^a }; say (&b,).raku;  # mutsu (,)     raku (-> $a { ... },)
```

## Related faces of the same rendering gap

```
my &b = { $^a }; say &b;          # mutsu sub { }   raku -> $a { #`(Block|...) ... }
sub f($x) {}; say (&f,);          # mutsu (f)       raku (&f)
say Str.^lookup('Int').gist;      # mutsu Int       raku method Int (Str:D $:: *%_ --> Int:D) { ... }
```

Three distinct defects: a bare `Block` is rendered as `sub { }` (wrong
declarator, and the signature is dropped); a named `Sub` inside a list loses its
`&` sigil; and a `Method`'s gist is just its name. The **list-element Block
collapsing to the empty string is the severe one**, because it changes how many
elements the reader believes the list has.

## Controls that are CORRECT

```
sub marine {}; say &marine.raku;  # sub marine { #`(Sub|...) ... }  -- matches raku's shape
sub f($x) {}; say &f;             # &f  -- correct when NOT in a list
```

So the `Sub` path is largely right and the `Block` path is not, and the
list-element collapse is specific to rendering inside a container.

## How this was found, and why it had been invisible

The doc-diff harness bucketed `Type/Code.rakudoc:140` as `raku-drift-from-doc`
— its low-priority bucket — because the doc's expected output embeds a Block
memory address (`Block|94212856419136`) that no run can ever reproduce. The
bucket name describes *raku vs the doc*, so a real mutsu divergence sitting
under it was never triaged. See the harness ticket
`doc-diff-harness-has-no-output-cap-or-nondeterminism-gate`.

## Acceptance

- `say (&b,)` shows the block, so the list does not read as empty.
- The bare-`Block` gist uses the `->`/`{...}` form with its signature, not
  `sub { }`.
- A named `Sub` inside a list keeps its `&`.
- The address inside `#`(Block|N)` obviously cannot match raku's; the pin must
  assert the *shape* (non-empty, starts with `->`, contains the parameter), not
  the exact string.
- A `t/` pin covering bare / in-a-list / in-an-array for `Block`, `Sub`, and
  `Method`.
