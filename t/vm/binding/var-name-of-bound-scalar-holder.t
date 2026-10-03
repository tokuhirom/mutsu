use Test;

# A `$` name `:=`-bound to a Scalar holder -- an array element, or another
# `$`-scalar -- aliases that Scalar, so `.VAR.^name` is `Scalar` even when the
# held value is a Hash/Array. Bound directly to an aggregate variable it has
# no Scalar of its own and reflects the aggregate (#11111). Measured on rakudo.

plan 7;

my %h = a => 1;

my @a; @a[0] = %h;
my $r := @a[0];
is $r.VAR.^name, 'Scalar', 'bound to an element holding a shared hash';

my @c = 5, %h;
my $u := @c[1];
is $u.VAR.^name, 'Scalar', 'bound to an element holding an itemized hash';

my $hi = %h;
my $t := $hi;
is $t.VAR.^name, 'Scalar', 'bound to a $-scalar holding a hash';

my @b = 1, 2;
my $s := @b[0];
is $s.VAR.^name, 'Scalar', 'bound to an element holding an Int';

my $w := %h;
is $w.VAR.^name, 'Hash', 'bound to a hash variable: the hash itself';
my @r = $w;
is @r.elems, 1, 'and it is not itemized on list assignment';

my @aa = 1, 2;
my $v := @aa;
is $v.VAR.^name, 'Array', 'bound to an array variable: the array itself';
