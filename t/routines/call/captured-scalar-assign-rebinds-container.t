use Test;

# A routine assigning to a captured scalar that holds an Array or a Hash
# rebinds the scalar; it never writes into the container the scalar held
# before (#16888, EEL 0.3's fork copier restoring `$alias = %state`).

plan 6;

my $e;
sub set-e($v) { $e = $v }
my %h = c => 1;
set-e(%h);
set-e({c => 9});
is-deeply %h, {c => 1}, 'a hash the scalar held keeps its contents';
is-deeply $e, {c => 9}, 'the scalar holds the new hash';

my $f;
sub set-f($v) { $f = $v }
my @a = 1, 2;
set-f(@a);
set-f([7]);
is-deeply @a, [1, 2], 'an array the scalar held keeps its elements';
is-deeply $f, [7], 'the scalar holds the new array';

my $g;
my &set-g = -> $v { $g = $v };
my $h1 = {c => 1};
set-g($h1);
set-g({c => 5});
$g<c>++;
is-deeply $h1, {c => 1}, 'a closure assignment rebinds too';

my %state = c => 0;
my $alias = %state;
sub assign-alias($v) { $alias = $v }
my $other = {c => 20};
assign-alias($other);
$alias<c> += 2;
is-deeply (%state, $other), ({c => 0}, {c => 22}), 'the aliased hash is left alone';
