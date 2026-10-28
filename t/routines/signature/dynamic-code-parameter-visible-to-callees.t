use Test;

# A `&*name` parameter is a dynamic variable: routines called from the one
# that bound it see it, also from a `gather` body forced further up.
plan 6;

sub inner() { &*CB(5) }
sub outer1($x, &*CB) { inner() }
is outer1(1, * + 1), 6, 'a callee reads the dynamic code parameter';

class P {
    method !get($o) {
        gather {
            my $f = sub ($v) { &*CB($v) };
            take $f($o);
            take $f($o + 1);
        }
    }
    method map($o, &*CB) { self!get($o).eager }
}
is-deeply P.new.map(1, * * 10).list, (10, 20), 'a gather body in a private method';

sub g3($o) { gather { take &*CB.defined; take &*CB($o) } }
sub m3($o, &*CB) { g3($o).eager }
is-deeply m3(1, * * 10).list, (True, 10), 'a gather forced by the binding routine';

sub g5($o) { (1, 2).map({ &*CB($_ + $o) }).eager }
sub m5($o, &*CB) { g5($o) }
is-deeply m5(2, * * 10).list, (30, 40), 'a map block';

sub g6($o) { gather { take $*DV($o) } }
sub m6($o, $*DV) { g6($o).eager }
is-deeply m6(3, * * 10).list, (30,), 'a scalar dynamic parameter keeps working';

sub g7($o) { gather { take @*DA.elems + $o } }
sub m7($o, @*DA) { g7($o).eager }
is-deeply m7(1, [1, 2, 3]).list, (4,), 'an array dynamic parameter in a gather';
