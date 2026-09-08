# The `list` listop stopped disarming a Seq's single-use gate

`list $s` on a single `Seq` is rakudo's no-op — `(list $s) =:= $s`, and nothing
about the Seq's iterator is touched. mutsu returned the same object too, but
reified it on the way in, and that reify permanently exempted the Seq from
every later consuming touch.

## Repro

```raku
my \s = (list 1..5).grep(* > 0);
sink s>>.abs;
sink s>>.abs;    # raku: X::Seq::Consumed   mutsu: dies too -- correct
```

```raku
my \s = (list 1..5).grep(* > 0);
my $x = list s;  # rakudo: a total no-op
sink s>>.abs;
sink s>>.abs;    # raku: X::Seq::Consumed   mutsu: no death   <- the bug
```

A single `list s`, anywhere before the two consuming touches, was enough.

## Root cause

ADR-0058 made `.map`/`.grep` deferred, so `try_native_function` reifies a
still-deferred map/grep Seq argument before handing it to pure Rust that reads
elements directly (`reify_map_grep_seq_args`). `SeqBody::reify` is documented as
"the ONLY thing that marks a body `retained`", and `retained` is exactly what
exempts a body from `take` — the consuming path that raises `X::Seq::Consumed`.

`builtin_list` never reads an element of a single `Seq` argument; it returns
`args[0].clone()`. So the reify bought nothing and cost the gate.

Found with `rust-gdb -batch` breaking on `seq_body.rs`'s retain assignment: the
backtrace named `try_native_function -> reify_map_grep_seq_args` directly, with
no guessing about which of the several plausible paths was responsible.

## Fix

Exempt exactly that shape — native function `list`, one argument, and that
argument tag-probes as a `Seq` — from the argument reify. With more than one
argument `list` genuinely flattens and does need the elements, so the exemption
is deliberately narrow.

Pinned by `t/list-listop-is-a-noop-on-a-seq.t` (8 tests, measured against raku).

## How it surfaced

`roast/S03-operators/context-forcers.t`'s "list listop doesn't cache" is
`dies-ok { sink (list seq)».abs, (list seq)».abs }`. It had been passing for the
wrong reason: mutsu's grep-capture merge clobbered a caller lexical, so the
block died of *that* instead. Fixing the capture merge removed the spurious
death and left the subtest honestly failing — a good illustration that a green
test is not by itself evidence the behaviour under it is right.

## Still divergent, not fixed here

`$s.list` twice and `$s.List` twice throw `X::Seq::Consumed` in raku and do not
in mutsu. That is the pre-existing `@$s` / `.list` parser ambiguity documented
on `reify_or_consume_seq_target_inner`'s `"list"` arm (mutsu desugars the array
deref `@$s` to the same method-name string, and `@$s` must stay re-readable),
not something this change touches.
