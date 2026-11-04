use Test;

# `#|` / `#=` comments on a parameter, an `enum` and a `subset` are printed as
# a trailing `.declarator-docs(...)` call on the node (#14880), like the docs
# of a sub or class (rakuast-named-declarator-docs.t). Shapes measured on
# rakudo 2026.09.

plan 5;

use experimental :rakuast;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }
sub stmt-raku(Str $src, Int $n = 0) { flat($src.AST.statements[$n].raku) }

is stmt-raku(Q"sub f(#| the arg
 $x) { }
"),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Sub.new( name => RakuAST::Name.from-identifier("f"), signature => RakuAST::Signature.new( parameters => ( RakuAST::Parameter.new( type => RakuAST::Type::Setting.new( RakuAST::Name.from-identifier("Any") ), target => RakuAST::ParameterTarget::Var.new( name => "\$x" ), optional => False ).declarator-docs( leading => ( "the arg", ) ), ) ), body => RakuAST::Blockoid.new( RakuAST::StatementList.new() ) ) )',
    'a leading doc on a parameter';
ok stmt-raku(Q"sub f($x #= trailing
) { }
").contains('.declarator-docs( trailing => ( "trailing", ) )'), 'a trailing doc on a parameter';
unlike stmt-raku(Q"sub f($x) { }
"), /'declarator-docs'/, 'an undocumented parameter has none';

is stmt-raku(Q"#| doc
enum E <a b>;
"),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Type::Enum.new( name => RakuAST::Name.from-identifier("E"), term => RakuAST::QuotedString.new( processors => <words val>, segments => ( RakuAST::StrLiteral.new("a b"), ) ) ).declarator-docs( leading => ( "doc", ) ) )',
    'a leading doc on an enum';
is stmt-raku(Q"#| doc
subset S of Int where * > 0;
"),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Type::Subset.new( name => RakuAST::Name.from-identifier("S"), where => RakuAST::ApplyInfix.new( left => RakuAST::Term::Whatever.new, infix => RakuAST::Infix.new(">"), right => RakuAST::IntLiteral.new(0) ), traits => ( RakuAST::Trait::Of.new( RakuAST::Type::Simple.new( RakuAST::Name.from-identifier("Int") ) ), ) ).declarator-docs( leading => ( "doc", ) ) )',
    'a leading doc on a subset';
