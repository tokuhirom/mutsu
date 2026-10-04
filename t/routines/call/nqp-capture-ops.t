use Test;
use nqp;

# nqp::usecapture / nqp::savecapture and the capture readers (#11496),
# checked against rakudo 2026.09. The capture is the call's raw arguments,
# whatever the signature did with them.

plan 23;

sub raw(|) { nqp::usecapture() }

{
    my $c := raw(1, 'b', k => 5);
    is nqp::captureposelems($c), 2, 'captureposelems counts the positionals';
    is nqp::captureposarg($c, 1), 'b', 'captureposarg';
    is nqp::capturehasnameds($c), 1, 'capturehasnameds';
    is nqp::captureexistsnamed($c, 'k'), 1, 'captureexistsnamed (passed)';
    is nqp::captureexistsnamed($c, 'z'), 0, 'captureexistsnamed (not passed)';
    is nqp::capturenamedshash($c)<k>, 5, 'capturenamedshash';
    is nqp::captureposprimspec($c, 0), 0, 'captureposprimspec: an object argument';
    throws-like { nqp::captureposarg($c, 5) }, Exception,
        message => 'Capture argument index (5) out of range (0..^2) for captureposarg',
        'an index out of range';
}

{
    my $e := raw();
    is nqp::captureposelems($e), 0, 'no arguments';
    is nqp::capturehasnameds($e), 0, 'no nameds';
}

{
    # The native readers want a native argument; these are objects.
    throws-like { nqp::captureposarg_s(raw('x'), 0) }, Exception,
        message => 'Capture argument is not a string argument for captureposarg_s',
        'captureposarg_s on an object argument';
    throws-like { nqp::captureposarg_i(raw(7), 0) }, Exception,
        message => 'Capture argument is not an integer argument for captureposarg_i',
        'captureposarg_i on an object argument';
    throws-like { nqp::captureposarg_n(raw(1.5e0), 0) }, Exception,
        message => 'Capture argument is not a number argument for captureposarg_n',
        'captureposarg_n on an object argument';
}

{
    # The raw arguments, not what the signature made of them.
    sub opt($a, $b?) { nqp::captureposelems(nqp::usecapture()) }
    is opt(1), 1, 'an omitted optional is not in the capture';
    is opt(1, 2), 2, 'a passed one is';
    sub slurpy(*@a) { nqp::captureposelems(nqp::usecapture()) }
    is slurpy(1, (2, 3), |(4, 5)), 4, 'a list stays one argument, a slip is flattened';
    sub named-only(:$x) { nqp::capturehasnameds(nqp::usecapture()) }
    is named-only(:x(1)), 1, 'a bound named argument is still in the capture';
}

{
    sub keep(|) { nqp::savecapture() }
    my $s := keep(7, 8, :x);
    is nqp::captureposelems($s), 2, 'savecapture outlives its frame';
    is nqp::captureposarg($s, 1), 8, 'with its arguments';
    is nqp::capturehasnameds($s), 1, 'and its nameds';
}

{
    my &block = -> $x { nqp::captureposelems(nqp::usecapture()) };
    is block(5), 1, 'a pointy block has its own capture';
    my class C { method m($x) { nqp::captureposelems(nqp::usecapture()) } }
    is C.m(3), 2, 'a method capture starts with the invocant';
}

{
    # Each call has its own capture: an inner call does not overwrite it.
    sub inner(|) { nqp::captureposelems(nqp::usecapture()) }
    sub outer(|) {
        my $n := inner(1, 2, 3);
        $n ~ '/' ~ nqp::captureposelems(nqp::usecapture())
    }
    is outer('a'), '3/1', 'nested calls keep separate captures';
}
