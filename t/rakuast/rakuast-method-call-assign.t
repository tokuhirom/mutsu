use Test;

# An assignment to a method call (`$o.x = v`) in RakuAST, measured on rakudo
# 2026.09: an `ApplyInfix` whose left side is the `ApplyPostfix` method call
# and whose infix is an `Assignment`. mutsu's parser expands it into an
# rw-accessor writeback; EVAL hands the method call back to the same expansion.

plan 9;

my $stmt = Q[my $o; $o.x = 3].AST.statements[1].expression;
isa-ok $stmt, RakuAST::ApplyInfix, '`$o.x = 3` is an ApplyInfix';
isa-ok $stmt.infix, RakuAST::Assignment, 'with an Assignment infix';
isa-ok $stmt.left.postfix, RakuAST::Call::Method, 'whose left side is the method call';

is EVAL(Q[class A { has $.x is rw }; my $o = A.new; $o.x = 3; $o.x].AST),
    3, 'an rw accessor assignment survives the round trip';
is EVAL(Q[class A2 { has $.x is rw }; my $o = A2.new; my $c = $o.x = 7; $c + $o.x].AST),
    14, 'so does one in expression position';
is EVAL(Q[my @a = 1, 2; @a.head = 9; @a.join(',')].AST),
    '9,2', 'a method returning a container is written through';
is EVAL(Q[my $s = "abc"; $s.substr-rw(0, 1) = "X"; $s].AST),
    'Xbc', 'a method with arguments writes back through its invocant';
is EVAL(Q[class B { has $!y; method s { self!y = 4; $!y }; method !y() is rw { $!y } }; B.new.s].AST),
    4, 'a private method call survives it';
throws-like { EVAL(Q[my $p = (a => 1); $p.value = 3].AST) }, X::Assignment::RO,
    'a read-only value still refuses the write';
