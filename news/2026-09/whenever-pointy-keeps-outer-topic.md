# `whenever` with a signature keeps the outer `$_`

A `whenever` block with a declared signature (`-> $x`, a sub-signature, or a
`$^v` placeholder) now binds the emitted value only through that signature.
`$_` inside the body stays the enclosing lexical topic, the same as for the
block passed to `.tap`:

```raku
$_ = "outer";
react whenever Supply.from-list(1) -> $x { say $_ }   # outer (was: 1)
```

The react drive loop used to hand every callback its value as a forced topic,
because a `Pair` passed as an ordinary positional argument would have bound as a
named argument instead. A callback with a signature now gets an emitted `Pair`
in its positional flavour, which is the conversion `f((a => 1))` already uses.
Only bare blocks and the `QUIT` phaser still get the value as their topic.

The callbacks are now compiled as blocks, not routines, which is what raku makes
them. So a `LAST` phaser in a bare-block `whenever` sees the enclosing `$_`
rather than a fresh `Any`, and `return` inside a `whenever` body returns from
the routine that encloses the `react`, as it does in raku. Before, the callback
itself caught the `return`, and the value was lost.

Closes #9595.
