use Test;

# Types `use NativeCall` exports (`my constant CArray is export = NativeCall::Types::CArray`)
# are types at parse time: `Type::Simple`, and `Type::Parameterized` when indexed (#18251).

plan 6;

sub init-of(Str $code, Int $stmt) {
    my $decl = $code.AST.statements[$stmt].expression;
    $decl.initializer.expression
}

my $pre = 'use NativeCall; ';

my $carray = init-of($pre ~ 'my $x = CArray;', 1);
is $carray.^name, 'RakuAST::Type::Simple', 'CArray is a Type::Simple';

my $ptr = init-of($pre ~ 'my $x = Pointer;', 1);
is $ptr.^name, 'RakuAST::Type::Simple', 'Pointer is a Type::Simple';

my $par = init-of($pre ~ 'my $x = CArray[uint16];', 1);
is $par.^name, 'RakuAST::Type::Parameterized', 'CArray[uint16] is a Type::Parameterized';
is $par.base-type.^name, 'RakuAST::Type::Simple', 'its base type is a Type::Simple';
is $par.args.args.elems, 1, 'one type argument';

my $pptr = init-of($pre ~ 'my $x = Pointer[int32];', 1);
is $pptr.^name, 'RakuAST::Type::Parameterized', 'Pointer[int32] is a Type::Parameterized';
