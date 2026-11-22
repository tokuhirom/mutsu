use Test;
use experimental :rakuast;

plan 13;

# `react` and `supply` over a bare statement hold the statement itself, and a
# same-line modifier is part of that statement, as in rakudo (#18801).

my @out;
react @out.push(1) if 1;
react @out.push(2) if 0;
is-deeply @out, [1], 'react STATEMENT if COND runs the react with the modified statement';

my @got;
(supply emit 5 if 1).tap: { @got.push($_) };
(supply emit 6 if 0).tap: { @got.push($_) };
is-deeply @got, [5], 'supply STATEMENT if COND';

sub blorst($code) { $code.AST.statements[0].expression.blorst }

is blorst('react say 1').^name, 'RakuAST::Statement::Expression', 'react STATEMENT';
is blorst('react say 1 if 1').^name, 'RakuAST::Statement::Expression', 'react STATEMENT if COND';
ok blorst('react say 1 if 1').condition-modifier.defined, 'the modifier is on the react statement';
is blorst('supply emit 1').^name, 'RakuAST::Statement::Expression', 'supply STATEMENT';
ok blorst('supply emit 1 if 1').condition-modifier.defined, 'the modifier is on the supply statement';
is blorst('supply whenever 1 { }').^name, 'RakuAST::Statement::Whenever', 'supply whenever';
is blorst('supply for 1 { emit $_ }').^name, 'RakuAST::Statement::For', 'supply for';
is blorst('supply { emit 3 }').^name, 'RakuAST::Block', 'supply BLOCK keeps its block';

is Q{react say 1 if 1}.AST.DEPARSE.trim, 'react say 1 if 1', 'react DEPARSE';

my @evaled;
Q{react @evaled.push(7) if 1}.AST.EVAL;
is-deeply @evaled, [7], 'EVAL of react STATEMENT if COND';
my @tapped;
Q{(supply emit 8 if 1).tap: { @tapped.push($_) }}.AST.EVAL;
is-deeply @tapped, [8], 'EVAL of supply STATEMENT if COND';
