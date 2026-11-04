use v6;
use Test;

# A bare `@` / `%` is a `state` variable of the enclosing block, like a bare
# `$`: it accumulates across calls and each occurrence is its own variable
# (#16524).

plan 8;

sub foo($x) { (@).push($x) }
my @seen = (foo($_).join(',') for ^3);
is-deeply @seen, ['0', '0,1', '0,1,2'], 'a bare @ accumulates across calls';

sub bar($x) { my $v = %; $v{$x} = $x; $v.keys.sort.join(',') }
@seen = (bar($_) for ^3);
is-deeply @seen, ['0', '0,1', '0,1,2'], 'a bare % accumulates across calls';

sub baz($x) { my $v = @; $v[$x] = $x; $v.join(',') }
@seen = (baz($_) for ^3);
is-deeply @seen, ['0', '0,1', '0,1,2'], 'an element assignment into a bare @ keeps its elements';

sub two($x) { ((@).push($x).join(','), (@).push($x * 10).join(',')) }
my @a = two(0);
my @b = two(1);
is-deeply @a, ['0', '0'], 'two bare @ in one routine are two variables (first call)';
is-deeply @b, ['0,1', '0,10'], 'two bare @ in one routine are two variables (second call)';

my $r1 = do { for ^2 { (@).push($_) } };
my @loop;
for ^3 { @loop.push((@).push($_).elems) }
is-deeply @loop, [1, 2, 3], 'a bare @ in a loop body is shared by the iterations';

sub fresh { my $c = sub { (@).push(1).elems }; $c() ~ $c() }
is fresh(), '12', 'a bare @ in a closure is shared by its calls';
is fresh(), '12', 'a new closure starts with an empty bare @';
