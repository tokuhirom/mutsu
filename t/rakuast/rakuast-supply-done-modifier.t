use v6;
use Test;

# `done VALUE` with a statement modifier is the same typed statement as the
# bare form (it expands to `emit VALUE; done`), and `.AST` shows rakudo's
# `done(VALUE)` call under the modifier.
plan 4;

my $taken = supply { emit 1; done 2 if True; emit 3 };
is-deeply $taken.list, (1, 2), '`done VALUE if COND` emits then finishes';

my $skipped = supply { emit 1; done 2 if False; emit 3 };
is-deeply $skipped.list, (1, 3), 'a false modifier does neither';

my $ast = Q|supply { done 7 }|.AST;
is $ast.statements.head.expression.WHAT.^name, 'RakuAST::StatementPrefix::Supply',
    'supply block parses to the supply prefix';
is-deeply EVAL(Q|my $s = supply { emit 4; done 5 }; $s.list|.AST), (4, 5),
    'a round trip through RakuAST keeps the emit and the done';
