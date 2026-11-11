use Test;

# A routine a role was mixed into keeps the role in place, so an `is rw`
# role attribute can be assigned through any expression that answers the
# routine, not only through the variable `does` was applied to (#17415).
# Checked against rakudo.

plan 5;

role Tally { has $.tally is rw }

class Holder {
    has $.code;
    method code-of { $!code }
}

my $h = Holder.new(code => sub { 1 });
$h.code-of does Tally;
$h.code-of.tally = 5;
is $h.code-of.tally, 5, 'through a method that answers the routine';
$h.code.tally = 6;
is $h.code.tally, 6, 'through an attribute accessor';

my $m = method () { 2 };
my @all = $m,;
@all[0] does Tally;
@all[0].tally = 7;
is $m.tally, 7, 'through an array element, seen by every alias';

my $s = sub { 3 };
$s does Tally;
sub get() { $s }
get().tally = 8;
is $s.tally, 8, 'through a call that answers the variable';

ok $h.code-of() ~~ Tally, 'the routine still does the role';
