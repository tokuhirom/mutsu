use Test;

# #12129: a `:=` rebind of a `module Foo { my ... }` / class-body `my` lexical
# installs a new container; a name bound to the old one keeps it.
plan 10;

module RbM {
    my $a := [1, 2];
    our sub take() { my $n := $a; $a := [7]; $n.elems }
    our sub cur() { $a.elems }
}
is RbM::take(), 2, 'module: earlier alias keeps the old container';
is RbM::cur(), 1, 'module: the lexical sees the rebind';

class RbC {
    my $buf := [1, 2];
    method take { my $n := $buf; $buf := []; $n.elems }
    method cur { $buf.elems }
}
is RbC.take, 2, 'class body: earlier alias keeps the old container';
is RbC.cur, 0, 'class body: the lexical sees the rebind';

module RbN {
    my @x := [1, 2, 3];
    our sub take() { my @n := @x; @x := [9]; @n.elems }
    our sub cur() { @x.elems }
}
is RbN::take(), 3, '@ lexical: earlier alias keeps the old container';
is RbN::cur(), 1, '@ lexical: the lexical sees the rebind';

# A method called while the body is still running must not be undone by the
# body's end-of-run writeback.
class RbB {
    my $buf := [1, 2];
    method take { my $n := $buf; $buf := []; $n.elems }
    method cur { $buf.elems }
    is RbB.take, 2, 'called from the body: alias keeps the old container';
    is RbB.cur, 0, 'called from the body: sees the rebind';
}
is RbB.cur, 0, 'rebind survives the body ending';

# Plain assignment still writes through the shared container.
module RbS {
    my $v = 10;
    our sub bump() { $v = $v + 1; $v }
    our sub cur() { $v }
}
RbS::bump();
is RbS::cur(), 11, 'assignment still stores through';
