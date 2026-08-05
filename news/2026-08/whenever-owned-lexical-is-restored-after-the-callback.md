# A `whenever` callback's owned lexical no longer leaks into the caller's env

Resolved by PR #5773/#5776, merged via campaign PR #5759.

A `supply { }` body's `my` declarations are its own lexicals, and the
`whenever` callbacks nested in it capture them as `authoritative_captures` —
installed with overwrite on entry so they win over a same-named caller lexical
when the callback is dispatched from the emitting thread
(`exec_whenever_scope_op`'s `owned_lexicals`). The bug was that the install
was never undone: the callback runs with the emitting thread's env, which for
an in-process `Supplier` is the main script's, so after the `react` block
finished the name was still bound in the main script.

```raku
class Zed { }
my $src = Supplier.new;
my $out = supply {
    my enum E <Zed R>;
    whenever $src.Supply { emit Zed.WHAT.^name }
};
react {
    whenever $out -> $v { say $v; done }          # E        (correct)
    whenever Promise.in(0.2) { $src.emit(1) }
}
say Zed.WHAT.^name;   # raku: Zed   mutsu before the fix: E
```

The symptom was visible only for a name with no local slot — an enum type or
variant, a `constant`, a lexical type. A `my $x` in the same position hid it
rather than avoiding it, because the caller reads `$x` from its own local
slot and never consults the polluted env entry.

The exit merges were already correct (`is_body_private` in
`src/runtime/resolution_call_sub.rs` refuses to write an owned name back to
the caller); what was missing was undoing the entry the authoritative install
itself put there. The shipped fix (`src/runtime/subtest.rs`,
`resolution_call_sub.rs`) restores authoritative captures by slot when the
callback returns and excludes read-only authoritative installs from broad
closure writeback, while retaining genuine free-variable writes.

`t/supply-block-enum-lexical.t` pins all three halves: the variant winning
over a same-named file-scope class inside the supply body and its `whenever`
callbacks (the exact shape Cro's request/response parsers use), the caller's
class being intact after the `react` block, and the block/sub non-leak cases.

The wider supply-block lexical-privacy campaign this belonged to is
`news/2026-08/supply-block-lexical-privacy.md`; the remaining sibling
writeback bugs are `todo/tickets/supply-block-lexical-leaks-through-thread-lane.md`
and `todo/tickets/supply-block-scalar-lexical-invisible-to-last-phaser.md`.
