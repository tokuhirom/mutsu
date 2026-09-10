use Test;

plan 4;

is -> &:(Str), 42 { 100 }(-> Str $v { $v.uc }, 42), 100,
    'anonymous callable pointy parameter supports sub-signature unpacking';

throws-like '-> &:(Int) {}({;})', X::TypeCheck::Binding::Parameter,
    'anonymous callable pointy parameter enforces signature constraints';

my sub testit(Int $v, :&fn:(Int)) { fn($v) }
my sub fn(Int $v) { $v * 2 }
my sub fn-str(Str $v) { $v ~ " * 2" }

is testit(42, :&fn), 84, 'named callable parameter accepts matching signature';
dies-ok { testit(13, fn => &fn-str) },
    'named callable parameter rejects mismatched signature';
