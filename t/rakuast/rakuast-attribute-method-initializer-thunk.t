use experimental :rakuast;
use Test;

# An attribute default's `Method::Initializer` thunk and a class attribute's
# `Method::ClassAccessor` (#18621), as rakudo 2026.11 visits them: a literal
# default stays in its `Trait::WillBuild` with an empty thunk visited beside
# it, any other default is computed by the thunk the `WillBuild` holds (and
# the initializer is not visited), and `my $.q` / `our $.q` has no
# `WillBuild` but a class accessor.

plan 7;

sub tree($src) {
    my $n = $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression;
    my @l;
    sub w($n, $d) { @l.push($n.^name.subst('RakuAST::','')); $n.visit-children({ w($_, $d + 1) }) }
    $n.visit-children({ w($_, 0) });
    @l.join(' ')
}
is tree(Q|class MI1 { has $.x = 5 }|), 'Initializer::Assign IntLiteral Method::Initializer Signature Parameter Parameter Type::Simple Name ParameterTarget::Var VarDeclaration::Simple Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Trait::WillBuild IntLiteral', 'a literal default: an empty thunk between initializer and WillBuild';
is tree(Q|class MI2 { has $.x = 1 + 2 }|), 'Trait::WillBuild Method::Initializer Signature Parameter Parameter Type::Simple Name ParameterTarget::Var VarDeclaration::Simple Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Statement::Expression Call::Name Name ArgList ApplyInfix Infix ArgList IntLiteral IntLiteral', 'a computed default: only the WillBuild thunk returning it';
is tree(Q|class MI3 { has Int $.r .= new(3) }|), 'Type::Simple Name Trait::WillBuild Method::Initializer Signature Parameter Parameter Type::Simple Name ParameterTarget::Var VarDeclaration::Simple Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Statement::Expression ApplyPostfix Type::Simple Name Call::Method Name ArgList IntLiteral', 'a .= default: the thunk calls the method on the type';
is tree(Q|class MI4 { has @.a = 1, 2 }|), 'Trait::WillBuild Method::Initializer Signature Parameter Parameter Type::Simple Name ParameterTarget::Var VarDeclaration::Simple Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Statement::Expression Call::Name Name ArgList ApplyListInfix Infix IntLiteral IntLiteral', 'a list default';
is tree(Q|class MI5 { my $.q = 5 }|), 'Initializer::Assign IntLiteral Method::ClassAccessor Name Signature Parameter Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Statement::Expression Var::Lexical', 'a class attribute: its Method::ClassAccessor';
is tree(Q|class MI6 { our @.p }|), 'Method::ClassAccessor Name Signature Parameter Type::Simple Name Parameter ParameterTarget::Var VarDeclaration::Simple Blockoid StatementList Statement::Expression Var::Lexical', 'an our attribute without a default';
is Q|class MI9 { has $.x = 1 + 2 }|.AST.statements[0].expression.body.body.statement-list.statements[0].expression.traits[0].raku.lines.grep(/'Call::Name' | 'return'/).elems, 2,
    'the computed default is returned by the thunk';
