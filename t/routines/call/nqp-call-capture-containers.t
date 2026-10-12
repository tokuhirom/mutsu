use Test;
use nqp;

# A call's recorded capture (`nqp::usecapture`, `nqp::p6argvmarray`) keeps
# the caller's containers, as MoarVM's does (#12839). The probes sit inside a
# block: Rakudo's optimizer passes a decontainerized value for a mainline
# `my` variable (and for a known callee's readonly parameter), which
# `raku --optimize=0` shows is an optimization, not the semantics.

plan 12;

{
    sub kind($a) { nqp::captureposarg(nqp::usecapture(), 0).VAR.^name }
    my $x = 1;
    my @a = 1;
    my %h = k => 1;
    is kind($x), 'Scalar', 'a variable argument is its Scalar';
    is kind(@a[0]), 'Scalar', 'an array element argument is its Scalar';
    is kind(%h<k>), 'Scalar', 'a hash element argument is its Scalar';
    is kind(5), 'Int', 'a literal argument is the value';
}

{
    sub argv-kind($a) { nqp::atpos(nqp::p6argvmarray(), 0).VAR.^name }
    my $x = 1;
    is argv-kind($x), 'Scalar', 'p6argvmarray keeps the container too';
}

{
    sub set-first($a) {
        my $c := nqp::captureposarg(nqp::usecapture(), 0);
        $c = 'written';
    }
    my $x = 1;
    set-first($x);
    is $x, 'written', 'assigning to the captured container writes the caller variable';
    my @a = 1, 2;
    set-first(@a[1]);
    is-deeply @a, [1, 'written'], 'and an element argument writes the array';
}

{
    sub same-name($x is copy) {
        $x = 99;
        nqp::captureposarg(nqp::usecapture(), 0)
    }
    my $x = 5;
    is same-name($x), 5, 'a same-named callee parameter does not alias the captured caller variable';
    is $x, 5, 'and the caller variable keeps its value';
}

{
    sub inner(*@v) { nqp::captureposelems(nqp::usecapture()) }
    sub outer($a) {
        my $n = inner(1, 2);
        my $c := nqp::captureposarg(nqp::usecapture(), 0);
        $c = 'outer';
        $n
    }
    my $x = 1;
    is outer($x), 2, 'a nested capture-reading call records its own capture';
    is $x, 'outer', 'and the outer capture still writes its own caller';
}

{
    sub lit($a) { nqp::captureposarg(nqp::usecapture(), 0) = 3 }
    throws-like { lit(5) }, Exception, 'a literal argument stays immutable';
}
