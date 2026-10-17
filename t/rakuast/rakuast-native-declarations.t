use v6;
use experimental :rakuast;
use Test;

plan 14;

is RakuAST::Native.^mro[1].^name, 'RakuAST::Package', 'Native inherits from Package';
my $constructed = RakuAST::Native.new(
    name => RakuAST::Name.from-identifier('ASTNativeConstructed'),
    repr => 'P6int',
);
isa-ok $constructed, RakuAST::Native, 'Native.new constructs the measured node class';

my $tree = Q[my native ASTNativeInt is Int is ctype<long> is repr<P6int> { }].AST;
my $native = $tree.statements[0].expression;
isa-ok $native, RakuAST::Native, 'a native package declarator has its own node';
is $native.name.canonicalize, 'ASTNativeInt', 'the native name is retained';
is $native.repr, 'P6int', 'the representation is a dedicated field';
is $native.traits.elems, 2, 'parent and ctype traits are retained in source order';
isa-ok $native.traits[0].type, RakuAST::Type::Simple, 'the parent is a type-valued Trait::Is';
is $native.traits[0].type.name.canonicalize, 'Int', 'the parent type is preserved';
is $native.traits[1].name.canonicalize, 'ctype', 'ctype stays a named Trait::Is';
nok $native.gist.contains('__mutsu_native_decl'), 'the execution marker stays out of the public tree';

my $unsigned = Q[my native ASTNativeUnsigned is Int is ctype<long> is unsigned is repr<P6int> { }].AST;
ok $unsigned.gist.contains('name => RakuAST::Name.from-identifier("unsigned")'),
    'unsigned is represented as a named trait';
my $size = Q[my native ASTNativeSize is Int is nativesize(8) is repr<P6int> { }].AST;
ok $size.gist.contains('name     => RakuAST::Name.from-identifier("nativesize")'),
    'nativesize is represented as a named trait';

is EVAL(Q[my native ASTNativeEval is Int is ctype<long> is repr<P6int> { }; ASTNativeEval.^nativesize]), -4,
    'a round-tripped native declaration applies ctype semantics';
is EVAL(Q[my native ASTNativeEvalUnsigned is Int is ctype<long> is unsigned is repr<P6int> { }; ASTNativeEvalUnsigned.^unsigned]), 1,
    'a round-tripped native declaration applies unsigned semantics';
