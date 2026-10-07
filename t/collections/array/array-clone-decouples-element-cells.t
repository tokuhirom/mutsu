use Test;

# Distilled from Terminal::UI (Frame.make-height-computer): `$in.clone` was
# re-run after a `for @$h.kv -> $i, $v is rw` loop had promoted the slots to
# shared cells, and the clone's writes reached the original array.

plan 4;

my $in = [2, :1fr];
for @$in.kv -> $i, $h is rw { }
my $c = $in.clone;
for @$c.kv -> $i, $h is rw { $h = 5 if $h ~~ Pair }
is-deeply $in, [2, :1fr], 'writes to the clone do not reach a promoted original';
is-deeply $c, [2, 5], 'the clone has the new values';

my $x = 5;
my @a = 1, 2;
@a[0] := $x;
my @b = @a.clone;
@b[0] = 9;
is $x, 5, 'a bound slot is not shared with the clone';

my $d = [1, 2];
for @$d.kv -> $i, $v is rw { }
my $e = $d.clone;
$e[0] = 9;
is-deeply $d, [1, 2], 'element assignment on a clone of a promoted array';
