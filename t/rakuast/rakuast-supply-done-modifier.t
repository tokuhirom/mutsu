use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# `done VALUE` is one call with its payload in RakuAST (`emit VALUE; done` is
# only how the executing tree spells it), also under a statement modifier.
plan 7;

my $ast = Q|supply { done 7 }|.AST;
my $body = $ast.statements[0].expression.blorst.body.statement-list;
is $body.statements.elems, 1, '`done VALUE` is a single statement';
my $call = $body.statements[0].expression;
is $call.^name, 'RakuAST::Call::Name::WithoutParentheses', 'a bare call';
is $call.name.canonicalize, 'done', 'named done';
is $call.args.args[0].value, 7, 'the payload is its argument';

is-deeply EVAL(Q|my $s = supply { emit 1; done 2 if True; emit 3 }; $s.list|.AST), (1, 2),
    '`done VALUE if COND` emits then finishes';
is-deeply EVAL(Q|my $s = supply { emit 1; done 2 if False; emit 3 }; $s.list|.AST), (1, 3),
    'a false modifier does neither';
is-deeply EVAL(Q|my $s = supply { emit 4; done 5 }; $s.list|.AST), (4, 5),
    'a round trip keeps the emit and the done';
