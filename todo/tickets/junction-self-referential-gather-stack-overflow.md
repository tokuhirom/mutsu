# A self-referential `$j = any (gather $j».take)...` combined with a value-producing `when` crashes with a stack overflow

Discovered via the doc-diff harness on `raku-doc/doc/Type/Junction.rakudoc` (around line 205).

## Repro (from the doc)

```raku
sub calc ($_) { die when 13 }
my $j = any 1..42;
$j = any (gather $j».take).grep: {Nil !=== try calc $_};
say so $j == 42;
```

- `raku`: `True`
- `mutsu` (`target/debug/mutsu`): `thread 'mutsu-main' has overflowed its stack` (exit 134)

## Narrowed repro

The crash does not need `die`/`try`; a plain sub containing a value-producing `when` (which
mutsu evaluates to `Nil` instead of raku's `Empty`/`()` on no-match — see the related
`control-do-when-expression-value.md` ticket) is enough, as long as it is combined with a
self-referential `$j` reassignment through `gather`/hyper-`.take`:

```raku
sub calc ($_) { 99 when 13 }
my $j = any 1..5;
$j = any (gather $j».take).grep: { Nil !=== calc $_ };
say $j == 3;   # crashes
```

Removing any one piece stops the crash:
- Replacing `calc` with a sub that has no `when` (e.g. `sub calc ($_) { 99 }` or `sub calc ($_)
  { Nil }`) → no crash (wrong result, but that's the separately-tracked Nil-vs-Empty `when`-value
  bug).
- Replacing the self-referential `gather $j».take` with a non-self-referential source
  (`$j.eigenstates`/a plain list) → no crash.
- Dropping the trailing `$j == 3` (i.e. never forcing/comparing the newly-built Junction) → no
  crash, `say "done"` prints fine.

## Root cause hypothesis (unconfirmed — needs a debugger session)

Raku evaluates the RHS of `$j = any (gather $j».take)...` **fully** (eagerly reifying the
`gather`'s lazy Seq, since Junction construction requires concrete eigenstates) before the
assignment to `$j` takes effect — so the `gather` block's `$j».take` still reads the *old* `$j`
(the plain `any 1..5`), never the value being constructed.

The crash pattern (only reproducing when a `when`-in-sub is in the pipeline, and only surfacing
later at `$j == 3` rather than at the assignment itself) suggests mutsu's `gather` here is not
being reified eagerly at Junction-construction time. If the lazy Seq underlying the new `$j` is
only forced later — at the `$j == 3` comparison — then by that point `$j` has already been
reassigned to the new (self-referential) Junction, so forcing the `gather` reads `$j».take`
against the *new*, not-yet-fully-constructed `$j`, recursing into itself without a base case →
stack overflow. It's unclear why the `when`-in-sub specifically is needed to trigger this
(possibly the `Nil`-vs-`Empty` return value takes a different code path through `.grep`'s lazy
machinery that skips whatever eager-reification the "no when" path performs), which needs a
`rust-gdb` session on the crash to confirm.

## Affected files (starting point)

- `src/runtime/methods_object_dispatch_new.rs` — the `Junction.new`/`any()`/`all()`/etc.
  eigenstate construction, to check whether it always eagerly reifies its Seq/gather argument
  before returning
- Regex/gather laziness: `runtime/iterator_protocol.rs`, the `gather`/`take` VM ops in
  `vm_control_ops.rs` — whether `gather`'s reification order relative to an enclosing assignment
  is correct
- Related, already-filed: `control-do-when-expression-value.md` (the `when`-as-expression
  Nil-vs-Empty gap that seems to be a necessary ingredient here, though not sufficient on its
  own — this crash needs the self-referential-Junction-via-gather shape too)
