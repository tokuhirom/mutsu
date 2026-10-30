use v6;
use Test;

# BitEnum / DB::Xoos (#15100): a `sub` in a parametric role body reads the
# composition's value parameters, one value per composition.

plan 7;

enum Bits (A => 1, B => 2);
role BE[::E, Str:D :$prefix = '', Bool :$lc] {
    sub lookup(Str:D $str is copy) { $str .= uc if $lc; "[$prefix|$str|{E.^name}]" }
    method look($s) { lookup($s) }
}
class K does BE[Bits, :prefix<SQ_>, :lc] { }
is K.new.look("ab"), '[SQ_|AB|Bits]', 'named value parameters and a type capture';
is BE[Bits, :prefix<P_>].new.look("cd"), '[P_|cd|Bits]', 'a punned parametric role';

role R2[Str:D :$prefix = ""] { sub f { "<$prefix>" }; method m { f() } }
class K2 does R2[:prefix<Q_>] { }
class K3 does R2[:prefix<Z_>] { }
is K2.new.m, '<Q_>', 'first composition';
is K3.new.m, '<Z_>', 'second composition keeps its own value';
is K2.new.m, '<Q_>', 'and the first is unchanged';

role R3[$p] { sub g { "<$p>" }; method m { g() } }
class K4 does R3["pp"] { }
is K4.new.m, '<pp>', 'a positional parameter';

role R4[Str :$tag = "t"] { my $n = 0; sub next-id { ++$n ~ $tag }; method id { next-id() } }
class K5 does R4[:tag<x>] { }
is (K5.new.id, K5.new.id).join(","), '1x,2x', 'body lexicals and parameters together';
