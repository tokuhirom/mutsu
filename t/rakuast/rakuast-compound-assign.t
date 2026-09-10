use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# Compound assignment keeps its RakuAST metaoperator instead of exposing the
# parser's execution-only assignment expansion.
plan 19;

my $ast = Q[my $x = 1; $x += 3].AST;
ok $ast.gist.contains('RakuAST::MetaInfix::Assign.new'),
    'compound assignment renders its metaoperator';
ok $ast.gist.contains('RakuAST::Infix.new("+")'),
    'compound assignment renders the base infix';
is $ast.statements[1].expression.infix.^name,
    'RakuAST::MetaInfix::Assign',
    'compound assignment exposes MetaInfix::Assign';
is $ast.statements[1].expression.infix.infix.gist,
    'RakuAST::Infix.new("+")',
    'compound assignment exposes its base infix node';

my $indexed = Q[my @a = 1; @a[0] += 3].AST;
is $indexed.statements[1].expression.infix.^name,
    'RakuAST::MetaInfix::Assign',
    'indexed compound assignment exposes MetaInfix::Assign';
is EVAL(Q[my @a = 1; @a[0] += 3; @a[0]].AST), 4,
    'EVAL of an indexed compound assignment executes';

my $at-pos = Q[my @a = 1; @a.AT-POS(0) += 2].AST;
is $at-pos.statements[1].expression.infix.^name,
    'RakuAST::MetaInfix::Assign',
    'AT-POS compound assignment exposes MetaInfix::Assign';
is EVAL(Q[my @a = 1; @a.AT-POS(0) += 2; @a[0]].AST), 3,
    'EVAL of an AT-POS compound assignment executes';

my $meta = RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new('+'));
isa-ok $meta, RakuAST::MetaInfix::Assign,
    'MetaInfix::Assign can be constructed';
is $meta.infix.gist, 'RakuAST::Infix.new("+")',
    'constructed MetaInfix::Assign exposes its infix';
ok 'new' (elem) RakuAST::MetaInfix::Assign.^methods(:local)>>.name
    && 'infix' (elem) RakuAST::MetaInfix::Assign.^methods(:local)>>.name,
    'MetaInfix::Assign advertises constructor and accessor';

is EVAL(Q[my $x = 1; $x += 3; $x].AST), 4,
    'EVAL of a constructed + assignment executes';
is EVAL(Q[my $x = 7; $x -= 2; $x].AST), 5,
    'EVAL of a constructed - assignment executes';
is EVAL(Q[my $x = 4; $x *= 3; $x].AST), 12,
    'EVAL of a constructed * assignment executes';
is EVAL(Q[my $x = 'a'; $x ~= 'b'; $x].AST), 'ab',
    'EVAL of a constructed ~ assignment executes';
is EVAL(Q[my $x; $x //= 9; $x].AST), 9,
    'EVAL of a constructed // assignment executes';
is EVAL(Q[my $x = 0; $x ||= 9; $x].AST), 9,
    'EVAL of a constructed || assignment executes';
is EVAL(Q[my $x = 1; $x &&= 9; $x].AST), 9,
    'EVAL of a constructed && assignment executes';
is EVAL(Q[my $x = 2; ($x += 3) * 2].AST), 10,
    'compound assignment remains usable in expression position';
