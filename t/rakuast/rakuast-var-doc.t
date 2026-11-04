use Test;

# `$=pod` and `$=finish` are `RakuAST::Var::Doc` nodes holding the name after
# `$=` (#14880); they used to read as a `Var::Lexical` named `$=pod`. Shapes
# measured on rakudo 2026.09.

plan 7;

use experimental :rakuast;

sub flat(Str $src) { $src.AST.statements.map(*.raku.subst(/\s+/, ' ', :g)).join(" | ") }

is flat('$=pod'),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Var::Doc.new( "pod" ) )',
    '$=pod reads as Var::Doc';
is flat('$=finish'),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Var::Doc.new( "finish" ) )',
    '$=finish reads as Var::Doc';
is '$=pod'.AST.statements[0].expression.name, 'pod', 'the name accessor';
is flat('my $n = $=pod.elems;'),
    'RakuAST::Statement::Expression.new( expression => RakuAST::VarDeclaration::Simple.new( sigil => "\$", desigilname => RakuAST::Name.from-identifier("n"), initializer => RakuAST::Initializer::Assign.new( RakuAST::ApplyPostfix.new( operand => RakuAST::Var::Doc.new( "pod" ), postfix => RakuAST::Call::Method.new( name => RakuAST::Name.from-identifier("elems") ) ) ) ) )',
    'inside a declaration';
is RakuAST::Var::Doc.new("pod").raku.subst(/\s+/, ' ', :g), 'RakuAST::Var::Doc.new( "pod" )',
    'a hand-built Var::Doc prints like the read one';
is RakuAST::Var::Doc.new("finish").name, 'finish', 'a hand-built name';

=begin pod
text
=end pod

my $ast = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Doc.new("pod"),
            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier("elems")))));
ok (EVAL $ast) ~~ Int, 'EVAL of a hand-built Var::Doc reads the Pod variable';
