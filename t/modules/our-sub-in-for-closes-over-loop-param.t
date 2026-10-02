use Test;

# An `our sub` declared in a mainline (GLOBAL) `for -> $i { ... }` body closes
# over the loop parameter: called after the loop, it sees the last
# iteration's binding, as rakudo does (#10647). A single `for` parameter has
# no slot (the loop binds it by name), so there is no declaration to box it at.

plan 6;

for 1..2 -> $i { our sub g { $i // 'u' } }
is GLOBAL::g(), 2, 'single loop parameter, called after the loop';

my @seen;
for 1..3 -> $n { our sub h { $n }; @seen.push(h()) }
is-deeply @seen, [1, 2, 3], 'called inside the loop, each iteration sees its own binding';
is GLOBAL::h(), 3, '... and the last one after the loop';

{ for 1..3 -> $p { our sub in-block { $p } } }
is GLOBAL::in-block(), 3, 'loop nested in a bare block';

for 1..4 -> $a, $b { our sub pair { "$a $b" } }
is GLOBAL::pair(), '3 4', 'multiple loop parameters';

for 1..3 -> $q { my $k = $q * 10; our sub mixed { "$q $k" } }
is GLOBAL::mixed(), '3 30', 'a loop parameter next to a `my` of the body';
