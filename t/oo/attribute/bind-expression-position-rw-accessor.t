use Test;

plan 5;

# Math::Symbolic (ecosystem): `if !(my $comm := $func.commute) && ... { $comm = ... }`
# -- a bind written inside an expression must alias an `is rw` attribute just
# as the statement form `my $c := $obj.attr` does.
class F { has $.commute is rw; has $.plain; }

my $f = F.new;
(my $a := $f.commute);
$a = 'one';
is $f.commute, 'one', 'parenthesized bind aliases the rw attribute';

my $g = F.new;
if !(my $b := $g.commute) { $b = 'two' }
is $g.commute, 'two', 'bind in an if condition';

my $h = F.new;
if !(my $c := $h.commute) && True { $c = 'three' }
is $h.commute, 'three', 'bind inside a larger condition';

my $i = F.new(commute => 'x');
my $r = (my $d := $i.commute);
$d = 'four';
is $i.commute, 'four', 'the bind is also the expression value';

my $j = F.new(plain => 5);
(my $e := $j.plain);
is $e, 5, 'a non-rw accessor still binds its value';
