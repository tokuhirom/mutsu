use Test;

# #18274 slice 3: a method called while its class body still runs writes the
# body's own `my` through the variable's cell.

plan 8;

class Assign {
    my $buf = 5;
    method take { $buf = 7 }
    method cur { $buf }
    Assign.take;
    is $buf, 7, 'the body sees the method\'s assignment';
    is Assign.cur, 7, 'so does another method';
}
is Assign.cur, 7, 'and it survives the body ending';

class Rebind {
    my $buf := [1, 2];
    method take { my $n := $buf; $buf := []; $n.elems }
    method cur { $buf.elems }
    is Rebind.take, 2, 'an earlier alias keeps the old container';
    is Rebind.cur, 0, 'the rebind is seen while the body runs';
}
is Rebind.cur, 0, 'and after it';

class Count {
    my $n = 0;
    method inc { $n++ }
    Count.inc for ^3;
    is $n, 3, 'repeated increments from the body accumulate';
}
Count.inc;
is Count.new.inc, 4, 'and continue after the body';
