# A `whenever` callback's owned lexical is left behind in the ambient env

A `supply { }` body's `my` declarations are its own lexicals, and the `whenever`
callbacks nested in it capture them as `authoritative_captures` (installed with
overwrite on entry, so they win over a same-named caller lexical when the
callback is dispatched from the emitting thread — `exec_whenever_scope_op`'s
`owned_lexicals`). The install is never undone: after the `react` block finishes,
the name is still bound in the main script's env.

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
say Zed.WHAT.^name;   # raku: Zed   mutsu: E
```

A `my $x` in the same position hides the symptom rather than avoiding it: the
caller reads `$x` from its own local *slot*, so a polluted env entry is never
consulted. A name with no slot — an enum type or variant, a `constant`, a
lexical type — has only the env entry, so the leak is visible.

## Where it comes from

`Interpreter::run_whenever_with_value` (`src/runtime/subtest.rs`) builds each
callback with `Value::make_sub_owning(..., owned_lexicals)`. Every exit merge
correctly refuses to write an owned name *back* to the caller
(`is_body_private` in `src/runtime/resolution_call_sub.rs` checks
`data.authoritative_captures`), but nothing removes the entry the authoritative
install itself put there. The callback runs with the emitting thread's env, which
for an in-process `Supplier` is the main script's.

The fix is to restore each authoritatively-installed name to its pre-entry value
(or remove it) when the callback returns, which means recording the pre-install
value alongside the install rather than only the name.

## Related

`t/supply-block-enum-lexical.t` pins the resolution half of this (an enum
declared in a supply body wins inside the whenever callback) and the block/sub
non-leak halves, but deliberately does not assert the post-`react` state.
`todo/deep/supply-block-lexicals-alias-the-caller.md` covers the wider aliasing
question this sits inside.
