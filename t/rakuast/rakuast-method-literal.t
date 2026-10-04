use Test;

# A method literal in RakuAST, measured on rakudo 2026.09: a nameless
# `RakuAST::Method` (or `Submethod`) whose signature lists only the written
# parameters. mutsu's parser prepends a synthetic receiver; `.AST` leaves it
# out and EVAL puts it back through the parser's own builder.

plan 9;

sub expr($src) { $src.AST.statements.head.expression }

isa-ok expr(Q[method ($n) { 2 }]), RakuAST::Method, '`method ($n) { }` is a Method';
isa-ok expr(Q[submethod { 2 }]), RakuAST::Submethod, '`submethod { }` is a Submethod';
is expr(Q[method ($a, $b) { 2 }]).signature.parameters.elems, 2,
    'the signature holds only the written parameters';
is expr(Q[method () { 2 }]).signature.parameters.elems, 0, 'an empty one has no parameters';

is EVAL(Q[my $v = 5; my $p := Proxy.new(FETCH => method () { $v }, STORE => method ($n) { $v = $n }); $p = 7; $v].AST),
    7, 'Proxy methods survive the round trip';
is EVAL(Q[my $m = method { self * 2 }; $m(3)].AST), 6, 'so does a bare method';
is EVAL(Q[my $m = method ($x) { self + $x }; $m(3, 4)].AST), 7, 'and one with a parameter';
is EVAL(Q[my $m = submethod (Int $x --> Int) { self * $x }; $m(3, 5)].AST),
    15, 'and a typed submethod with a return type';
is EVAL(Q[class C { has $.v }; my $m = method { $.v + 1 }; $m(C.new(v => 41))].AST),
    42, 'the receiver is still `self`';
