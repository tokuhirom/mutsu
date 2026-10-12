# `&?BLOCK` survives repeated calls and `&?ROUTINE` is the routine itself

A named sub that read `&?BLOCK` got `Nil` from its second call on, because the
light call path skips the frame that holds the code object; reading `&?BLOCK`
or `&?ROUTINE` now keeps the routine on the frame-pushing path. `&?ROUTINE` of
a named sub also resolves to the same object as `&name`, so `&?ROUTINE === &f`
and equal `.WHICH` hold as in Rakudo.
