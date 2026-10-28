# An unbounded closure sequence no longer pre-computes 32 elements

`2, { $_**2 - $_ + 1 } ... *` ran its generator 32 times when the sequence was
created, so a generator whose values grow doubly exponentially (Sylvester's
sequence, as in `Math::Sequences`) hung on bigints, and a side-effecting
generator ran ahead of demand. Rakudo computes elements only when asked; so
does mutsu now (#15306).
