use Test;
plan 8;

# A unit with a CHECK/INIT phaser reorders its top-level statements; the
# staging temp and the targets of `my ($a, $b) = RHS` must keep their order.
my @m; CHECK { @m.push: 1; @m.push: 2 }
INIT { 1 }

my ($p, $q) = @m;
is "$p $q", "1 2", '`my ($p, $q) = @array` after a CHECK-filled array';

my ($r) = (5,);
is $r, 5, 'a one-element destructuring declaration';

my ($s, $t) = 3, 4;
is "$s $t", "3 4", 'a comma-list right-hand side';

my ($u, $v) = @m.list;
is "$u $v", "1 2", 'a method-call right-hand side';

my ($w, @rest) = 7, 8, 9;
is "$w @rest[]", "7 8 9", 'a slurpy target';

my %h = a => 1;
my ($k) = %h.keys;
is $k, "a", 'a hash-derived right-hand side';

my $x = 9;
is $x, 9, 'a plain declaration is unaffected';

my ($a, $b);
($a, $b) = @m;
is "$a $b", "1 2", 'a separate declaration and assignment still works';
