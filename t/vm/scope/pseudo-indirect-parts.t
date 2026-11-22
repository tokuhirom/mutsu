use Test;

# `::($a)::($b)` is one symbolic name (rakudo: Term::Name(EmptyEdge,
# Expression($a), Expression($b))), looked up at the site that wrote it, so a
# `CALLER`/`CALLERS`/`OUTER` head walks the caller frames or the scope chain
# from there. Roast S02-names/pseudo-6d.t's 116, 125 and 139 (#18742).

plan 14;

sub f1($f) { my $x is dynamic = 90; my $*bar = 93; $f() }
sub f2($f) { my $x is dynamic = 91; f1($f) }
my $caller = 'CALLER';
my $callers = 'CALLERS';
my $outer = 'OUTER';

is f1({ ::($caller)::('$*bar') }), 93, '::("CALLER")::<$*bar>';
is f1({ CALLER::('$*bar') }), 93, 'CALLER::(\'$*bar\')';
my $*foo = 92;
is f2({ ::($callers)::('$*foo') }), 92, '::("CALLERS")::<$*foo>';

sub h2($f) { my $*y = 101; $f() }
{
    my $*y = 107;
    is h2({ ::($outer)::('$*y') }), 107, '::("OUTER")::<$*y> is the lexical outer';
    is h2({ OUTER::('$*y') }), 107, 'OUTER::(\'$*y\') too';
}

{ our $x37 = 38; }
package A36 { our $x38 = 39; }
my $our = 'OUR';
is ::($our)::('$x37'), 38, '::("OUR")::<$x>';
is ::($our)::('A36')::('$x38'), 39, 'a package part-way is searched through its stash';
my $pkg = 'A36';
is ::($pkg)::('$x38'), 39, '::($pkg)::<$x>';
nok ::($our)::('$nope').defined, 'a missing name is undefined';

# A routine bound into the setting's stash is the routine seen where no
# declaration shadows it.
sub g1(|) { 'g1' }
sub g2(|) { 'g2' }
&CORE::none := &g1;
ok &none =:= &g1, '&CORE::none := &g1 rebinds &none';
CORE::.<&none> := &g2;
ok &none =:= &g2, 'CORE::.<&none> := &g2 rebinds &none';
{
    sub none(|) { 'mine' }
    is &none.name, 'none', 'a lexical declaration still shadows it';
}

# A bare `OUTER::` is the pad one scope out.
{
    my $w = 'really unlikely value';
    {
        my $v = 1;
        ok OUTER::.values.grep({ ($_ // '') eq 'really unlikely value' }),
            'OUTER::.values produces the outer values';
        nok OUTER::.keys.grep('$v'), '... and not the inner names';
    }
}
