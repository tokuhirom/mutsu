use Test;

plan 13;

# A defaulted array declared in EXPRESSION position takes the same default-first
# route as the statement form (#10318): the value of `(my @a is default(D) = ...)`
# is the declared container, not a re-coerced copy of the initializer list.
my $r = (my @z is default(1) = Nil, Any);
is $r.raku, '$[1, Any]', 'my: a Nil item uses the default and an explicit Any stays Any';
is @z.raku, '[1, Any]', 'my: the declared variable holds the same elements';
is @z[5], 1, 'my: the declared variable keeps the default';

is (my @q is default(1) = Nil, Any).raku, '[1, Any]',
    'my: used directly as a method invocant';

my @all;
for 1..2 { @all.push: (my @a is default(3) = $_, Nil, Any) }
is @all.raku, '[[1, 3, Any], [2, 3, Any]]', 'my: a fresh container on every evaluation';

my @outer = 1;
{
    my $s = (my @outer is default(4) = Nil, 2);
    is $s.raku, '$[4, 2]', 'my: a shadowing expression declaration applies its own default';
}
is @outer.raku, '[1]', 'my: the shadowed outer array is untouched';

my $t = (my Int @typed is default(5) = 1, Nil);
is $t.raku, 'Array[Int].new(1, 5)', 'my: a typed defaulted array keeps its element type';

# `state` in expression position: the same value, initialized once.
my $s = (state @w is default(1) = Nil, Any);
is $s.raku, '$[1, Any]', 'state: a Nil item uses the default and an explicit Any stays Any';

my $inits = 0;
sub reenter {
    my $x = (state @keep is default(7) = do { ++$inits; Nil }, Any);
    @keep.push(@keep.elems);
    $x.raku ~ ' ' ~ @keep.raku
}
# `$x` is the declared container itself, so it sees the later `push` too.
is reenter(), '$[7, Any, 2] [7, Any, 2]', 'state: the first entry initializes';
is reenter(), '$[7, Any, 2, 3] [7, Any, 2, 3]', 'state: the container persists across entries';
is $inits, 1, 'state: the initializer runs only on the first entry';

sub plain-state { my $x = (state @p is default(7) = 5, 6); @p.push(1); @p.raku }
plain-state();
is plain-state(), '[5, 6, 1, 1]', 'state: a defaulted array keeps what earlier calls pushed';
