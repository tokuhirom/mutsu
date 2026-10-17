use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 12;

my $ast = Q[my @a = 1, 2; @a Z= 3, 4].AST;
my $assignment = $ast.statements[1].expression;
isa-ok $assignment, RakuAST::ApplyListInfix,
    'Z= is represented as a list infix application';
isa-ok $assignment.infix, RakuAST::MetaInfix::Zip,
    'Z= exposes the zip metaoperator';
isa-ok $assignment.infix.infix, RakuAST::Assignment,
    'the zip metaoperator wraps Assignment';
ok $assignment.infix.infix.gist.contains(':item'),
    'zip assignment uses item assignment semantics';
is EVAL(Q[my @a = 1, 2; @a Z= 3, 4; @a.join(',')].AST), '3,4',
    'ordinary array zip assignment replaces corresponding elements';
is EVAL(Q[my @a = 1, 2, 3; @a Z= 8; @a.join(',')].AST), '8,2,3',
    'zip assignment preserves the unmatched tail';

my $decl = Q[my @a Z= 3, 4].AST.statements[0].expression;
isa-ok $decl, RakuAST::ApplyListInfix,
    'declaration Z= is one list infix application';
isa-ok $decl.operands[0], RakuAST::VarDeclaration::Simple,
    'the declaration is the first operand';
is EVAL(Q[my @a Z= 3, 4; @a.elems].AST), 0,
    'declaration zip assignment preserves the fresh empty array';

my $shaped = Q[my int @native[2;3] Z= 0..5].AST.statements[0].expression;
isa-ok $shaped.operands[0], RakuAST::VarDeclaration::Simple,
    'shaped native declaration remains the first operand';
is-deeply EVAL(Q[my int @native[2;3] Z= 0..5; (@native[0;0], @native[1;2])].AST),
    (0, 5), 'shaped native zip assignment fills in row-major order';
is EVAL(Q[my @a = 1, 2; @a Z= 3, 4; @a[1]].AST), 4,
    'EVAL of the round-tripped assignment retains indexed values';
