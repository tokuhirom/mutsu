use Test;

# A list element that is a `my $x = v` declaration holds the declared Scalar
# itself, as a plain `$x` element does: a later write to the variable shows
# through the list. Parentheses around an element do not decontainerize
# either (#18230).

plan 12;

my @l = (my $s = 0; $s = 5; 1);
is @l.raku, '[5, 5, 1]', 'a statement list holds the declared container';

my $r = (my $t = 1; $t++; 2);
is $r.List.raku, '(2, 1, 2)', 'a later increment shows through';

my $q = ((my $y = 1), 2);
$y = 5;
is $q.List.raku, '(5, 2)', 'a parenthesized declaration in a comma list';

my $x = 1;
my $r2 = (($x), 2);
$x = 5;
is $r2.List.raku, '(5, 2)', 'a parenthesized variable keeps its container';

sub run { (my $sum = 0; for ^4 { $sum += $_ }; $sum) }
is run()[0], 6, 'the first element of a routine result is the final sum';

my @a = (my $z = 3; 4);
$z = 9;
is @a.raku, '[3, 4]', 'assigning to an array copies the values';

my @seen;
for ^2 { my $l = (my $k = $_; $k++; 0); @seen.push: $l[0] }
is @seen.raku, '[1, 2]', 'each loop iteration declares a fresh container';

my $m = (my Int $ti = 1; 2);
$ti = 7;
is $m.List.raku, '(7, 2)', 'a typed declaration holds its container';

my $ni = (my int $nn = 1; 2);
$nn = 7;
is $ni.List.raku, '(1, 2)', 'a native declaration has no container to hold';

my $c = ((my $w = 1), 2);
$c[0] = 8;
is $w, 8, 'writing through the list element writes the variable';

my $b = (my $bi := 1; 2);
is $b.List.raku, '(1, 2)', 'a bind declaration yields its value';

my $an = (my $ = 1; 2);
is $an.elems, 2, 'an anonymous declaration is still one element';
