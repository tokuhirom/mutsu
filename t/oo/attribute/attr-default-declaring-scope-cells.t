use Test;

# An attribute default names the lexicals of the scope it was declared in:
# a write reaches that lexical, a read sees its later writes, and the
# constructing frame's same-named lexical is not it (#13340).

plan 10;

my $count = 0;
role R3 { has $.n = ++$count }
class E does R3 {}
is (E.new.n xx 3).join(','), '1,2,3', 'a role default counts up';
is $count, 3, 'and its writes reach the role scope lexical';

my $c2 = 0;
class C2 { has $.n = ++$c2 }
sub make-c2() { C2.new.n }
is make-c2(), 1, 'a class default constructed from a routine';
is $c2, 1, 'writes the class scope lexical';

my $later = 1;
class L { has $.v = $later }
$later = 5;
is L.new.v, 5, 'a read sees a write made after the declaration';

my $k3;
{
    my $c3 = 10;
    class C3 { has $.n = ++$c3 }
    is C3.new.n, 11, 'constructed in the declaring block';
    $k3 = -> { $c3 };
}
is C3.new.n, 12, 'constructed after the declaring block exited';
is $k3(), 12, 'the block lexical saw both writes';

sub shadow() { my $c2 = 'not this'; C2.new.n }
is shadow(), 2, 'the constructing frame\'s same-named lexical is not used';

my $m;
{
    my $f = 10;
    role MR { has $.n = ++$f }
    $m = class M2 does MR {}
}
is $m.new.n, 11, 'a block role composed in the block, constructed outside';
