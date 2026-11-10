use Test;

plan 9;

# A `$` parameter typed with a container role holds the whole container: the
# role says what the argument is, not what its elements are.
sub store-key(Associative $p) { $p<k> = 1; $p }
sub store-index(Positional $p) { $p[0] = 4; $p }
sub store-where(Associative $p where { $_ !~~ Positional }) { $p<k> = 5; $p }
sub store-defined(Associative:D $p) { $p<k> = 6; $p }

my %h;
store-key(%h);
is-deeply %h, {k => 1}, 'an Associative parameter: the element is assigned';

my @a;
store-index(@a);
is-deeply @a, [4], 'a Positional parameter';

my %w;
store-where(%w);
is-deeply %w, {k => 5}, 'an Associative parameter with a where clause';

my %d;
store-defined(%d);
is-deeply %d, {k => 6}, 'a definite Associative parameter';

# The same through a multi method with the attach shape Qwiratry's navigators use.
class N {
    multi method attach(Associative $parent where { $_ !~~ Positional && $_ !~~ Str }, Str $step, Mu $child) {
        $parent{$step} = $child;
        $child
    }
    multi method attach(Positional $parent where { $_ !~~ Str }, Str $step, Mu $child) {
        $parent.push($child);
        $child
    }
}
my $n = N.new;
my %m;
my @p;
is $n.attach(%m, 'k', 1), 1, 'a hash target';
is $n.attach(@p, 'k', 2), 2, 'an array target';
is-deeply %m, {k => 1}, 'the hash got the key';
is-deeply @p, [2], 'the array got the child';

# A typed container is still checked against its element type.
my Int %typed;
dies-ok { %typed<x> = 'text' }, 'a typed hash still checks its elements';
