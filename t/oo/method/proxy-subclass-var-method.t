use Test;

# A method a `Proxy` subclass declares, called on the Proxy's `.VAR` object,
# runs the subclass's method and sees and updates the subclass's own
# attributes. From AttrX::Mooish's AttrProxy
# (`$attr-var.VAR.is-set`, `.VAR.build-acquire`, ...).
plan 10;

my class P is Proxy {
    has $!set is default(False);
    method is-set { ⚛$!set }
    method mark { $!set ⚛= True; self }
    method twice($n) { $n * 2 }
    method named(:$x = 1) { $x + 1 }
    method Str { 'P-Str' }
}

my $p := P.new(FETCH => -> $ { 42 }, STORE => -> $, $ { });

nok $p.VAR.is-set, 'a subclass method on .VAR reads the attribute default';
$p.VAR.mark;
ok $p.VAR.is-set, 'and sees an update made through another method';
is $p.VAR.twice(21), 42, 'positional arguments';
is $p.VAR.named(:x(4)), 5, 'named arguments';
is $p.VAR.named, 2, 'named default';
is $p, 42, 'the Proxy still FETCHes as a value';
is $p.VAR.Str, 'P-Str', 'a subclass override of a built-in method name';
ok $p.VAR.WHAT =:= P, '.VAR is the Proxy container, not the FETCHed value';

my $count = 0;
for ^200 { $count++ if $p.VAR.is-set }
is $count, 200, 'repeated calls';

my class Q is P { method is-set { 'q:' ~ callsame } }
my $q := Q.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $q.VAR.is-set, 'q:False', 'a further subclass override with callsame';
