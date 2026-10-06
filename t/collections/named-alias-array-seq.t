use Test;

plan 5;

sub c(:edges(:@x)!) { @x.elems }
is c(edges => (1..3).map({ %(a => $_) })), 3, 'alias key binds a Seq to the @ leaf';
is c(x => (1..3).map({ %(a => $_) })), 3, 'inner name binds a Seq to the @ leaf';
is c(x => [1, 2]), 2, 'an Array still binds';
sub d(:edges(:@x)) { @x.elems }
is d(edges => (1..4).map(* * 2)), 4, 'optional alias binds a Seq';
dies-ok { c(x => 5) }, 'a non-Positional is still rejected';
