use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

# A colonpair argument in a parameterized type spelling
# (`has NameTree[Dest, :coerce(&f)] $.x`, PDF::Class PDF::Names) survives
# `.AST` as a `ColonPair::Value` argument and lowers back to the same program.

plan 3;

my $src = Q|role RtNT[::T, :$coerce] { method c { $coerce.defined ?? $coerce(2) !! 0 } }
sub rt-double($x) { $x * 2 }
class RtHolder { has RtNT[Int, :coerce(&rt-double)] $.x; }
class RtHolder2 { has RtNT[Int, :!coerce] $.y; }|;

my $ast = $src.AST;
like $ast.gist, /'ColonPair::Value'/, 'the :coerce(&f) argument is a ColonPair::Value';
like $ast.gist, /'ColonPair::False'/, 'the :!flag argument is a ColonPair::False';
lives-ok { EVAL $ast }, 'the lowered program evaluates';
