use Test;

# `if COND -> PARAMS { }` (and `elsif`) in RakuAST, measured on rakudo 2026.09:
# the clause body is a `PointyBlock` whatever its parameter looks like
# (sigilless, `@`-sigilled, typed, `$_`). EVAL of the tree binds the
# parameters to the condition value as the parsed program does.

plan 17;

sub then-of($src) { $src.AST.statements[0].then }

my $term = then-of(Q[if 5 -> \y { y }]);
isa-ok $term, RakuAST::PointyBlock, 'a sigilless `if` pointy is a PointyBlock';
isa-ok $term.signature.parameters[0].target, RakuAST::ParameterTarget::Term,
    'its parameter is a ParameterTarget::Term';

my $arr = then-of(Q[if 5 -> @p { 1 }]);
isa-ok $arr, RakuAST::PointyBlock, 'an `@` parameter is a PointyBlock';
is $arr.signature.parameters[0].target.name, '@p', 'named `@p`';

my $typed = then-of(Q[if 5 -> Int $n { $n }]);
isa-ok $typed, RakuAST::PointyBlock, 'a typed parameter is a PointyBlock';
is $typed.signature.parameters[0].target.name, '$n', 'named `$n`';

my $topic = then-of(Q[if 5 -> $_ { 1 }]);
is $topic.signature.parameters[0].target.name, '$_', '`-> $_` keeps the topic parameter';

my $elsif = Q[if 0 { 1 } elsif 5 -> \z { z }].AST.statements[0].elsifs[0].then;
isa-ok $elsif, RakuAST::PointyBlock, 'an `elsif` pointy is a PointyBlock';

isa-ok then-of(Q[if 5 { 1 }]), RakuAST::Block, 'the parameterless form stays a Block';

sub run($src) { EVAL($src.AST) }
is run(Q[my $r; if 7 -> \y { $r = y * 2 }; $r]), 14, 'a sigilless parameter is bound';
is run(Q[my @a = 1, 2; my $r; if @a -> @p { @p.push(3); $r = @p.join(",") }; $r]), '1,2,3',
    'an array parameter is bound';
is run(Q[my $r; if 3 -> Int $n { $r = $n + 1 }; $r]), 4, 'a typed parameter is bound';
is run(Q[my $r; $_ = "outer"; if 4 -> $_ { $r = $_ }; $r ~ $_]), '4outer',
    '`-> $_` is a fresh topic';
is run(Q[my $r; if (1, 2) -> ($a, $b) { $r = $a + $b }; $r]), 3,
    'a destructuring parameter unpacks the condition';
is run(Q[my $r; if 0 { $r = 0 } elsif 6 -> \z { $r = z }; $r]), 6, 'an `elsif` sigilless parameter';
is run(Q[my $r = "no"; if 0 -> \y { $r = "yes" }; $r]), 'no', 'a false condition skips the body';
is run(Q[sub f { if 8 -> \v { v + 1 } }; f()]), 9, 'the clause value is the last statement';
