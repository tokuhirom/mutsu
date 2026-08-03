# A supply block's captured lexicals survive dispatch from a foreign frame

Two live instances of one `supply { … }` parse site shared their captures:

```raku
sub xform($tag, Supply $in --> Supply) {
    supply whenever $in -> $v { emit $tag ~ '(' ~ $v ~ ')' }
}
my $src = Supplier.new;
my $s = xform('a', $src.Supply);
$s = xform('b', $s);
$s.tap(-> $v { say $v });
$src.emit('x');            # Rakudo: b(a(x)) — mutsu printed a(a(x))
```

The outer instance's `whenever` callback read the *inner* instance's `$tag`.

## Why

A `whenever` callback is dispatched much later, from whichever frame is
emitting. `resolution_call_sub`'s merge gives the *calling* frame priority for
any captured name that is not in the closure's authoritative set — deliberately,
so a `Proxy` FETCH body and friends see live values. `exec_whenever_scope_op`
counters that for supply blocks by handing each callback an `owned_lexicals`
list, but that list held only the body's `my` declarations (and, since the
previous fix, its emitter). `$tag` is a free variable of the block, captured from
a `xform` frame that has already returned, so it fell through to caller priority
— and with one parse site instantiated twice, the "caller" is the sibling
instance, whose binding has the very same name.

The compiler already computes exactly the right set: `authoritative_free_vars`,
the captures the *creating frame* vouches it never writes after the capture op.
The problem was that `exec_whenever_scope_op` sees the chunk `eval_block_value`
re-compiled from the lambda's AST, and that chunk has no enclosing frame to
vouch for anything, so its `authoritative_free_vars` was always empty.

## Fix

The vouched set now travels with the supply-body mark, the same way
`supply_emitter_sym` does: `resolution_call_sub` copies the compiled lambda's
`authoritative_free_vars` into `pending_supply_authoritative_free_vars`, and
`resolution_eval` merges them into the re-compiled chunk.
`exec_whenever_scope_op` adds them to `owned_lexicals`.

Using the vouched set rather than all free variables is what keeps the two
opposite cases working, both pinned in the new test:

- a capture the *caller* reassigns after building the block must be read live
  (`my $gate = 0; my $sup = supply { … emit $gate … }; $gate = 9`), and
- a capture the *body* writes must reach the declaring frame
  (`whenever $s.on-close({ $closed = True })`).

Owning either by name would be a by-value snapshot that silently goes stale;
the compiler's vouch excludes both.

Pinned by `t/supply-block-captured-lexical.t`.
