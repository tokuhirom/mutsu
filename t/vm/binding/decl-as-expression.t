use Test;

plan 11;

# `(with ...)` / `(without ...)` / `(given ...)` are statements usable as expressions,
# just as `(for ...)` already was.
is (with 1 -> $a { $a * 2 }), 2, 'with is usable as an expression';
is (with 21 { $_ * 2 }), 42, 'with without a pointy binds the topic';
is (without Nil { 'empty' }), 'empty', 'without is usable as an expression';
is (given 6 { $_ * 7 }), 42, 'given is usable as an expression';
is-deeply (for 1 -> $a { $a * 2 }), (2,), 'for is still usable as an expression';

# A sigil needs no space after the declarator.
my@i = 1, 2, 3;
is-deeply @i, [1, 2, 3], 'my@i declares an array';
is <a b c>[my$n = 1], 'b', 'my$n declares inside a subscript';

# A declaration can be the lvalue of a compound assignment. Unlike `=`, the value has to
# read the variable back, so a `state` initializer would be wrong here: the assignment
# must run on every pass, not once.
(my $n) += 5;
is $n, 5, '(my $n) += 5 assigns to the fresh declaration';

my @maxes;
for 1, 5, 3 {
    (state $best) max= $_;
    @maxes.push($best);
}
is-deeply @maxes, [1, 5, 5], '(state $best) max= runs on every pass';

# The state variable stays in the enclosing scope, so a later statement sees it.
my $seen;
for 1, 5, 3 {
    (state $top) max= $_;
    LAST $seen = $top;
}
is $seen, 5, 'a declaration in a compound assignment is visible to later statements';

# `my(...)` is still a call to a keyword-named sub, not a declaration.
sub my(|c) { 'called' }
is my('x'), 'called', 'my(...) still calls a keyword-named sub';
