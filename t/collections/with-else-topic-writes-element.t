use Test;

# From CSS::TagSet (CSS::Grammar::AST): the else block of `with %h<k> { } else
# { $_ = v }` topicalizes the element, so assigning `$_` stores into it.
plan 5;

my %g;
for 1, 2 -> $x {
    with %g<a> { .push($x); next }
    else { $_ = [$x] }
}
is-deeply %g, {a => [1, 2]}, 'first pass stores, second pushes';

my %h;
with %h<k> { fail 'unreachable' } else { $_ = 5 }
is-deeply %h, {k => 5}, 'hash element';

my @a;
with @a[1] { fail 'unreachable' } else { $_ = 7 }
is-deeply @a, [Any, 7], 'array element';

my %k = k => 1;
with %k<k> { $_ = 2 } else { $_ = 3 }
is-deeply %k, {k => 2}, 'then-branch unchanged';

# A caller's typed `$_` parameter must not coerce the else block's topic.
sub fill(IO() $_) {
    my %e;
    with %e<k> { fail 'unreachable' } else { $_ = [5] }
    %e
}
is-deeply fill('a.txt'), {k => [5]}, 'untyped topic inside a routine with IO() $_';
