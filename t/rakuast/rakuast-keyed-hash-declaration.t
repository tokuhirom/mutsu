use Test;

# A key-typed hash declaration in RakuAST, measured on rakudo 2026.09: the key
# type is the declaration's `shape` (a SemiList holding the type) and a
# written value type is its `type`. mutsu's parser folds both into one type
# string and records when it supplied the `Any` value type itself.

plan 11;

sub decl($src) { $src.AST.statements.head.expression }

my $bare = decl(Q[my %h{Any}]);
isa-ok $bare.shape, RakuAST::SemiList, '`my %h{Any}` has a shape';
isa-ok $bare.shape.statements.head.expression, RakuAST::Type::Simple, 'holding the key type';
nok $bare.type, 'and no value type';

my $typed = decl(Q[my Int %h{Str}]);
isa-ok $typed.type, RakuAST::Type::Simple, '`my Int %h{Str}` keeps its value type';
ok $typed.shape.statements.head.expression.raku.contains('"Str"'), 'beside its key type';

ok decl(Q[my Any %h{Int}]).type.raku.contains('"Any"'),
    'a written `Any` value type stays written';

is EVAL(Q[my %h{Any}; %h{1} = 2; %h.keys.head.^name].AST),
    'Int', 'an object hash survives the round trip';
is EVAL(Q[my Int %g{Str} = a => 1; %g.of.^name ~ ' ' ~ %g.keyof.^name].AST),
    'Int Str', 'so do a value type and a key type';
is EVAL(Q[my Int %g{Str}; try { %g<b> = "x" }; $!.^name].AST),
    'X::TypeCheck::Assignment', 'and the value type still constrains a store';
is EVAL(Q[my Any %h{Int}; %h.keyof.^name].AST),
    'Int', 'a written Any value type survives it';
is EVAL(Q[my %h{Str:D}; %h.keyof.^name].AST),
    'Str:D', 'a definite key type survives it';
