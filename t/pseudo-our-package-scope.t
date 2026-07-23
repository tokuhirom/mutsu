use v6;
use Test;

# OUR:: pseudo-package is the current package's own stash: at file scope it is
# GLOBAL, inside `package A {}` it is A. So `$OUR::x` / `OUR::.{}` / `::('OUR')`
# resolve the CURRENT package's `our` variables and sub-packages, and a GLOBAL
# `our` stays invisible from a nested package (roast 6.c/S02-names/pseudo-6c.t).

plan 12;

# --- file (GLOBAL) scope -----------------------------------------------------
{
    { our $x30 = 31; our $x32 = 33; our $x34 = 35; }
    my $x = 39;

    is $OUR::x30, 31, 'basic $OUR:: read at file scope';
    $OUR::x30 := $x;
    ok $OUR::x30 =:= $x, 'can bind through $OUR::';

    is OUR::.<$x32>, 33, 'OUR::.<> read at file scope';
    OUR::.<$x32> := $x;
    ok $OUR::x32 =:= $x, 'can bind through OUR::.<>';

    my $our = 'OUR';
    is ::($our)::('$x34'), 35, '::("OUR") indirect read at file scope';
}

# --- nested package scope ----------------------------------------------------
{
    { our $g99 = 99; }              # a GLOBAL our
    my $our = 'OUR';

    our package A36 {
        { our $x37 = 38; }

        ok !defined($OUR::g99),   '$OUR:: does not find GLOBAL from a package';
        is  $OUR::x37, 38,        '$OUR:: finds the current package var';
        ok !defined(OUR::.<$g99>), 'OUR::.<> does not find GLOBAL from a package';
        is  OUR::.{'$x37'}, 38,   'OUR::.<> finds the current package var';
        ok !defined(::($our)::('$g99')), '::("OUR") does not find GLOBAL';
        is  ::($our)::('$x37'), 38, '::("OUR") finds the current package var';
    }

    is $OUR::A36::x37, 38, '$OUR:: can indirect through a sub-package';
}
