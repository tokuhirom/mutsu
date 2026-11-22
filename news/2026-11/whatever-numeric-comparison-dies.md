# A Whatever value in a numeric comparison dies

In Rakudo, `my $w = *; $w == 1` dies with
`Cannot resolve caller Numeric(Whatever:D: )`, and `$w < 1` fails the same way
through `Real`. `Whatever` has no numeric coercion candidate. mutsu's
arithmetic operators already died, but the numeric comparisons answered as if
the `Whatever` were `0` (`$w == 1` was `False`) (#18766).

The comparison operators' operand check now refuses a bare `Whatever` or
`HyperWhatever` value, as it refuses a `Sub`. A `*` written as an operand is
still curried at parse time (`* == 1`, `1 < * < 3`).
