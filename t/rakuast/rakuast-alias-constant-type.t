use Test;

# A constant whose initializer is a type (or a parameterization of one) is a
# type at parse time: a later bareword is `Type::Simple` and an index over it
# a `Type::Parameterized`. Measured on Rakudo 2026.11 (#18252).

plan 6;

sub init-of(Str $code, Int $stmt) {
    $code.AST.statements[$stmt].expression.initializer.expression
}

my $pre = 'use NativeCall; ';

my $idx = init-of($pre ~ 'constant CA2 = CArray[uint8]; my $x = CA2[uint16];', 2);
is $idx.^name, 'RakuAST::Type::Parameterized', 'an index over a parameterized alias is a Type::Parameterized';
is $idx.base-type.^name, 'RakuAST::Type::Simple', 'whose base is a Type::Simple';

is init-of($pre ~ 'constant CA2 = CArray; my $x = CA2[uint16];', 2).^name,
    'RakuAST::Type::Parameterized', 'an alias of an imported type is indexed as a type';
is init-of('constant A = Array; my $x = A[Int];', 1).^name,
    'RakuAST::Type::Parameterized', 'an alias of a setting type is indexed as a type';
is init-of('constant A = Array[Int]; my $x = A;', 1).^name,
    'RakuAST::Type::Simple', 'a parameterized alias names a type';
is init-of('constant N = 5; my $x = N;', 1).^name,
    'RakuAST::Term::Name', 'a value constant stays a term';
