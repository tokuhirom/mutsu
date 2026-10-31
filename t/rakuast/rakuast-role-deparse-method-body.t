use Test;
use experimental :rakuast;

# A role body carries the statements rakudo's tree adds around its own (a
# `Role::ResolveInstantiations` first, the fabricated return value last), from
# the parser and from `RoleBody.new` alike. `RakuAST::Deparse` reads a role body
# as `statements.skip.head(*-1)`, so a body without them lost its methods when
# deparsed (#15549). The constructor form and the lowering leave them out.
# Expected values are rakudo 2026.09's.

plan 11;

my $role = Q|role R { method r { 1 } }|.AST.statements[0].expression;
my @statements = $role.body.body.statement-list.statements;
is @statements.map(*.^name).join(' '),
    'RakuAST::Role::ResolveInstantiations RakuAST::Statement::Expression RakuAST::Statement::Expression',
    'a parsed role body holds the implicit statements';
is @statements[1].expression.^name, 'RakuAST::Method', 'the method sits between them';

is Q|role R { method r { 1 } }|.AST.DEPARSE, "role R \{\n    method r \{\n        1\n    }\n}\n",
    'the role keeps its method when deparsed';
is Q|role R { }|.AST.DEPARSE, "role R \{ }\n", 'an empty role deparses empty';
is Q|role R[::T] { method r { 1 }; has $.x }|.AST.DEPARSE.lines.grep(/method|has/).elems, 2,
    'a parameterized role keeps its members when deparsed';

my $class = Q|class C { method r { 1 } }|.AST.statements[0].expression;
is $class.body.body.statement-list.statements.elems, 1, 'a class body has no implicit statements';

my $method = RakuAST::Method.new(
    name => RakuAST::Name.from-identifier("r"),
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)))));
my $built = RakuAST::Role.new(
    name => RakuAST::Name.from-identifier("R"),
    body => RakuAST::RoleBody.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $method)))));
is $built.body.body.statement-list.statements.map(*.^name).join(' '),
    'RakuAST::Role::ResolveInstantiations RakuAST::Statement::Expression RakuAST::Statement::Expression',
    'RoleBody.new adds the implicit statements';
is $built.DEPARSE, "role R \{\n    method r \{\n        1\n    }\n}", 'a hand-built role deparses with its method';

is Q|role R { method r { 1 } }|.AST.raku.lines.grep(/ResolveInstantiations|MetaObject|curlexpad/).elems, 0,
    '.raku leaves the implicit statements out';

role S { method s { 42 } }
class UsesS does S { }
is UsesS.new.s, 42, 'a declared role still composes';
is EVAL(Q|role T { method t { 7 } }; class UsesT does T { }; UsesT.new.t|), 7, 'an EVAL string with a role still runs';

done-testing;
