use Test;

plan 5;

# A sigilless loop parameter named like a term (`i`, `e`) is the loop
# variable inside the body, not the term.
my @seen;
for reverse 0 .. 2 -> UInt \i { @seen.push: i }
is-deeply @seen, [2, 1, 0], 'typed `-> UInt \i` reads the loop value';

sub in-sub { my @r; for 1, 2 -> \i { @r.push: i + 1 }; @r }
is-deeply in-sub(), [2, 3], 'sigilless `\i` loop parameter inside a sub';

class Walker { method walk { my @r; for 3, 4 -> \i { @r.push: i }; @r } }
is-deeply Walker.walk, [3, 4], 'sigilless `\i` loop parameter inside a method';

my @closures;
for 1, 2 -> \i { @closures.push: { i } }
is-deeply @closures.map({ $_() }).List, (1, 2), 'a closure captures the loop parameter';

is i, Complex.new(0, 1), 'outside the loop `i` is still the imaginary unit';
