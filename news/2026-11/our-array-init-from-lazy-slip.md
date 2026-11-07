# `our @a = |(1..*)` keeps the array lazy

`my @a = |(1..*)` kept the infinite range lazy, but the same initializer on an
`our` array (the by-name store path) wrapped the lazy tail in a one-element
array whose element printed as `(...)`. The store now treats a one-item Slip of
an unreifiable lazy list as the `my` path does. Math::Sequences'
`our @A000027 is export = |ℕ` is correct again.
