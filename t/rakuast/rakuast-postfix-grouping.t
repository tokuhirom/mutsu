use experimental :rakuast;
use Test;

plan 5;

sub run($source) { EVAL($source.AST) }

my $grep = Q[(1..6).grep(* %% 2)].AST.statements[0].expression;
is $grep.operand.^name, 'RakuAST::ApplyInfix',
    'the postfix operand omits the visible parentheses';
is run(Q[(1..6).grep(* %% 2).join(' ')]), '2 4 6',
    'the grouped range remains the method receiver';

is run(Q[my $bag = (orange => 1, apple => 3).Bag; $bag{()}.elems]), 0,
    'empty parentheses inside a Bag subscript select no keys';

is run(Q[{ my @a = 1..6; for @a.grep(* %% 2) { $_ *= 10 }; @a.join(' ') }]),
    '1 20 3 40 5 60', 'compound assignment writes back through grep';

is run(Q[{ my @a = 1..6; for @a.grep(* %% 2) { $_ *= 10 } }
         { my @log; for (1..6).grep({ @log.push("g$_"); $_ %% 2 }) {
             @log.push("b$_"); last if $_ == 4
           }; @log.join(' ') }]),
    'g1 g2 b2 g3 g4 b4',
    'a prior compound assignment does not force a later lazy grep';
