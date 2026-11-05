use Test;

plan 10;

# `$a is copy` copies the Scalar, not the Array or Hash it holds.
my @x = 1, 2;
sub push-it($a is copy) { $a.push(3) }
sub same-as-x($a is copy) { $a === @x }
push-it(@x);
is @x.raku, '[1, 2, 3]', 'push through a $ copy reaches the caller array';
ok same-as-x(@x), 'a $ copy of an array is the same array';

my %h = a => 1;
sub put-it($h is copy) { $h<b> = 2 }
put-it(%h);
is %h.keys.sort.join(','), 'a,b', 'a $ copy of a hash is the same hash';

sub reassign($a is copy) { $a = [9]; $a }
my @y = 1, 2;
reassign(@y);
is @y.raku, '[1, 2]', 'assigning the copied Scalar leaves the caller alone';

# The `@` and `%` forms still copy the container.
sub at-copy(@a is copy) { @a.push(3); @a.elems }
my @z = 1, 2;
is at-copy(@z), 3, '@a is copy gets a fresh array';
is @z.elems, 2, '... and the caller keeps its own';
sub pct-copy(%h is copy) { %h<b> = 2; %h.elems }
my %g = a => 1;
is pct-copy(%g), 2, '%h is copy gets a fresh hash';
is %g.elems, 1, '... and the caller keeps its own';

# Binding a `but`-mixed Array to an @ variable (#15429).
role M { }
my $m = [5, 6, 7] but M;
my @b := $m;
is @b.elems, 3, 'a mixed itemized Array binds to @b';
ok @b ~~ M, '... and keeps its role';
