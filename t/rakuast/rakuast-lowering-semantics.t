use Test;

# Three constructs whose tree is the same as the parser's but whose meaning
# the lowering used to lose, so EVAL of `.AST` computed something else:
#
# - a chained comparison, left-nested in rakudo and chained by the infix's
#   associativity (`0 <= 9 < 3` is False, not `True < 3`);
# - the `::T` type capture of a pointy block, which has to stay bound;
# - `$.x = v` as a statement, which writes through the attribute's accessor.

plan 14;

sub run($src) { EVAL($src.AST) }

is run(Q[my $i = 9; (0 <= $i < 3).Str]), 'False', 'a chain is not a comparison of a boolean';
is run(Q[my $i = 2; (0 <= $i < 3).Str]), 'True', 'and holds when both links do';
is run(Q[(1 < 2 < 3 < 4).Str]), 'True', 'a chain of three';
is run(Q[(1 < 2 < 3 > 4).Str]), 'False', 'whose last link fails';
is run(Q[my $n = 0; sub f { $n++; 2 }; my $r = 1 < f() < 3; "$r $n"]), 'True 1',
    'the middle operand is evaluated once';
is run(Q[(3 !before 2 before 1).Str]), 'False', 'a negated link';
is run(Q[(1 !before 2 before 3).Str]), 'False', 'in the first place';
is run(Q[(3 !before 2 !before 1).Str]), 'True', 'and twice';
is run(Q[((0 <= 9) < 3).Str]), 'True', 'a parenthesized left side is a plain comparison';

is run(Q[my &id = -> ::T { T }; id(Int).^name]), 'Int', 'a pointy block binds its `::T`';
is run(Q[my &kind = -> ::T $x { T.^name }; kind(5)]), 'Int', 'also with a parameter after it';

is run(Q[class ItDec { has $.cur is rw; method dec { $.cur = $.cur - 1; $.cur } }
         my $i = ItDec.new(cur => 3); $i.dec; $i.dec]), 1,
    '`$.x = v` assigns through the accessor';
is run(Q[class ItSet { has $.cur is rw; method set($v) { $.cur = $v } }
         my $i = ItSet.new(cur => 3); $i.set(8); $i.cur]), 8, 'with a plain value';
is run(Q[class ItIf { has $.cur is rw; method dec { $.cur = $.cur - 1 if $.cur > 0; $.cur } }
         my $i = ItIf.new(cur => 1); $i.dec; $i.dec]), 0, 'under a statement modifier';
