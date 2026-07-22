use v6;
use Test;

plan 10;

# A bare `if EXPR { ... $^a ... }` block receives the condition value as its
# scalar placeholder, like `if EXPR -> $a { ... }`. Regression: `$^a` used to
# read the boolified condition (True) instead of the value. The bug bit hardest
# for a compile-time-constant condition (`if 42 { ... }`), which was resolved at
# compile time and inlined without binding the placeholder.

# Statement form (non-last statement, so it takes the const-folding path).
my @out1;
$_ = 1;
if 42 { @out1.push($_); @out1.push($^a) }
is @out1, [1, 42], 'bare if block: $_ stays outer, $^a is the condition value';

# do if / value forms.
is (do if 5 { $^a * 2 }), 10, 'do if: $^a binds the condition (arithmetic)';
is (do if 9 { $^a + 1 }), 10, 'do if: $^a in a value expression';

# Parenthesised value-if.
my $r = (if 7 { $^a - 2 });
is $r, 5, 'parenthesised (if ...) binds $^a';

# elsif branch binds its own $^a to the elsif condition.
my $ev;
if 0 { $ev = 'no' } elsif 3 { $ev = $^a }
is $ev, 3, 'elsif block binds its own $^a to the elsif condition';

# Not-taken then-branch: placeholder binding must not fire, else runs.
is (if 0 { $^a } else { 'else' }), 'else', 'untaken then-branch: else value returned';

# The condition value flows unflattened (a list condition stays whole).
my $lst;
if (1, 2, 3) { $lst = $^a }
is $lst.elems, 3, 'a list condition binds whole to $^a';

# `$_` inside the block is NOT the condition (stays the outer topic).
$_ = 99;
my $topic;
if 42 { $topic = $_ }
is $topic, 99, 'bare if does not set $_ to the condition';

# A non-constant condition takes the runtime branch and still binds.
my $rc;
my $cond = 8;
if $cond { $rc = $^a }
is $rc, 8, 'runtime (non-constant) condition binds $^a';

# Nested if: the inner $^a binds the inner condition, not the outer.
my $inner;
if 2 { if 5 { $inner = $^a } }
is $inner, 5, 'nested if: inner $^a binds the inner condition';
