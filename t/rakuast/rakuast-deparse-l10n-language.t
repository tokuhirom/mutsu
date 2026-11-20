use Test;
use experimental :rakuast;

# `.DEPARSE("XX")` loads `RakuAST::Deparse::L10N::XX` (rakudo's
# `RakuAST::Deparse.slang`) and mixes it into the deparser, so the keywords come
# out translated (#14845). The fixture has the shape the L10N::XX distributions
# generate; measured against rakudo 2026.11 with the same fixture.
use lib 't/lib';

plan 4;

my $ast = q:to/END/.AST;
my $x = 42;
if $x > 1 { say "big" } else { say "small" }
for 1..3 -> $i { say $i }
sub foo(Int $a) { return $a + 1 }
class Foo { has $.a; method bar { self.a } }
END

is $ast.DEPARSE("XT"), q:to/END/, 'the keywords are translated';
mein $x = 42;
wenn $x > 1 {
    sag "big"
}
sonst {
    sag "small"
}
fuer 1 .. 3 -> $i {
    sag $i
}
unter foo (Int $a) {
    zurueck $a + 1
}
klasse Foo {
    hat $.a;
    methode bar {
        selbst.a
    }
}
END

is $ast.DEPARSE.lines[0], 'my $x = 42;', 'without a language the source is English';
is Q[say 1].AST.DEPARSE("XT").trim, 'sag 1', 'a single statement';
is Q[say 1].AST.DEPARSE(RakuAST::Deparse.slang("XT")).trim, 'sag 1', 'the role from .slang mixes in the same way';
