use Test;

# An indexed bind (`@a[i] := v`, `%h<k> := v`) in RakuAST, measured on rakudo
# 2026.09: an `ApplyInfix` with a plain `:=` infix over the subscript. mutsu's
# parser builds a bind-marked `IndexAssign`; EVAL hands the subscript back to
# the same builder.

plan 10;

my $bind = Q[my @a; @a[0] := 5].AST.statements[1].expression;
isa-ok $bind, RakuAST::ApplyInfix, '`@a[0] := 5` is an ApplyInfix';
isa-ok $bind.infix, RakuAST::Infix, 'with a plain infix';
isa-ok $bind.left, RakuAST::ApplyPostfix, 'over the subscript';
isa-ok $bind.left.postfix, RakuAST::Postcircumfix::ArrayIndex, 'an array index';

is EVAL(Q[my @a = 1, 2; my $x = 5; @a[0] := $x; $x = 6; @a.join(',')].AST),
    '6,2', 'an array element bind survives the round trip';
is EVAL(Q[my %h; my $y = 1; %h<a> := $y; $y = 2; %h<a>].AST),
    2, 'so does a hash element bind';
is EVAL(Q[my %h; %h{"k"} := 4; %h<k>].AST),
    4, 'and one through a braced subscript';
is EVAL(Q[my %h; my $c = %h<k> := 7; $c + %h<k>].AST),
    14, 'an indexed bind in expression position survives it';
is EVAL(Q[my %h; %h<a> := %h<b> := 7; %h<a> + %h<b>].AST),
    14, 'so does a chained one';
is EVAL(Q[my @a; @a[0] := * + 1; @a[0](2)].AST),
    3, 'a WhateverCode right side stays a WhateverCode';
