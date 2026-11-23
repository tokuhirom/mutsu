use Test;

# The RakuAST accessors whose declaring class moved between rakudo 2026.09
# and 2026.11, answered from the classes 2026.11 declares them in (#19059).

plan 9;

is RakuAST::MetaInfix::Cross.^find_method('infix').package.^name, 'RakuAST::MetaInfix::CrossZip',
    'Cross.infix comes from CrossZip';
is RakuAST::Declaration::Import.^find_method('lexical-name').package.^name, 'RakuAST::Declaration::External',
    'Import.lexical-name comes from External';

my $c = RakuAST::VarDeclaration::Implicit::Constant.new(:name<$x>, :value(42));
is $c.name, '$x', 'Implicit::Constant.name';
is $c.scope, 'my', 'Implicit::Constant.scope defaults to my';
is $c.value, 42, 'Implicit::Constant.value';
is RakuAST::VarDeclaration::Implicit::Constant.new(:name<$y>, :value(1), :scope<our>).scope, 'our',
    'a given scope is kept';
is RakuAST::VarDeclaration::Implicit::Self.new.name, 'self', 'Implicit::Self.name';

my $i = RakuAST::Declaration::Import.new(:lexical-name<&foo>, :compile-time-value(7));
is $i.lexical-name, '&foo', 'a built Import keeps its lexical-name';
is $i.compile-time-value, 7, 'and its compile-time-value';
