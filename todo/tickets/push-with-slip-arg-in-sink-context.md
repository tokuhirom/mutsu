# `push(@a, 1, |@rest)` fails with "Unknown call: push" in sink context

## Symptom

A `push` whose argument list contains a slip (`|...`) resolves to nothing at
all — but only when its result is discarded:

```raku
my @a; push(@a, 1, |(2,3)); say @a.raku;   # mutsu: Unknown call: push
my @a; push(@a, |(2,3));    say @a.raku;   # mutsu: Unknown call: push
my @a; push @a, 1, |(2,3);  say @a.raku;   # mutsu: Unknown call: push  (listop form too)
```

raku prints `[1, 2, 3]` for all three. Binding the result makes mutsu work:

```raku
my @a; my $r = push(@a, 1, |(2,3)); say @a.raku;   # [1, 2, 3]  — correct
```

and so does dropping the slip:

```raku
my @a; push(@a, 1, 2); say @a.raku;                # [1, 2]     — correct
```

So the failing combination is precisely **slip argument + sink context**. The
error is `Unknown call: push`, i.e. dispatch never finds the routine, rather
than a type or arity complaint from inside it.

## Where to look

`push` is a listop with a dedicated fast path; the sink-context compilation of
a statement-level call is a different opcode path from the value-producing one
(see `builtins_collection_listops.rs` and the listop arms in
`vm_call_func_ops.rs`). The likely shape of the bug is that the sink path's
arity/candidate lookup counts the *un-expanded* slip argument, so the arity it
looks up has no registered candidate — the value path expands first and finds
one. Confirm with `--dump-bytecode` on the two spellings (`push(...)` alone vs
`my $r = push(...)`), which should differ in exactly one opcode.

`todo/deep/listops-are-not-real-multi-subs.md` is adjacent background: listops
are special-cased rather than being ordinary multi candidates, which is what
makes two dispatch paths able to disagree like this in the first place.

## Discovered via

`Config::TOML`'s `Actions.rakumod` (`push(@step-taken, $step, |pwd($root,
@rest))`, three sites) — `special-cases/02-arraytable-table-repeat.rakutest`
and `special-cases/03-txn.rakutest` both die with this. See
`docs/batteries/toml.md`.
