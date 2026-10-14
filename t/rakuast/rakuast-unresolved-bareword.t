use Test;

# An unresolved bare word has no RakuAST tree in rakudo (`.AST` throws
# X::Undeclared::Symbols). mutsu renders it as an argument-less
# `Call::Name` and keeps it a bare word when lowered, so it evaluates the
# way the parsed program does (an undeclared-name error, not an unknown call).

plan 5;

my $ast = Q|Foo|.AST;
isa-ok $ast, RakuAST::StatementList, 'a lone unresolved word has an AST';
my $call = $ast.statements[0].expression;
isa-ok $call, RakuAST::Call::Name, 'rendered as a Call::Name';
nok $call.args.defined && $call.args.args.elems, 'with no arguments';
is $call.name.canonicalize, 'Foo', 'carrying the word';
throws-like { Q|Foo|.AST.EVAL }, X::Undeclared::Symbols,
    'EVAL of it reports the undeclared name like a parsed program';
