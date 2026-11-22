# A declaration inside a list holds its Scalar

A list element that is a `my $x = v` declaration now holds the declared
Scalar container, as a plain `$x` element already did, so a later write to
the variable shows through the list:

```raku
my @l = (my $s = 0; $s = 5; 1); say @l;   # [5 5 1], was [0 5 1]
my $r = (my $t = 1; $t++; 2);   say $r;   # (2 1 2), was (1 1 2)
```

The same element wrapped in parentheses (`((my $y = 1), 2)`, `(($x), 2)`)
keeps its container too, because parentheses do not decontainerize. A native
declaration (`my int $n`) and a `:=` bind still yield the value, as in
rakudo (#18230).
