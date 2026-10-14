# Empty semicolon array shapes are ignored with a warning

An empty shape declaration such as `my @a[;]` now follows Rakudo's behavior: the parser emits the
`Ignoring [Nil] as shape specification` warning and declares an ordinary unshaped array. The array
can then grow through indexed assignment as usual.
