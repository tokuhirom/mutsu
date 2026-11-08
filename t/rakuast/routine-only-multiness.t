use Test;
use experimental :rakuast;

plan 10;

# `only` is the default dispatch declarator, but rakudo's tree records it as
# `multiness => "only"` and `.DEPARSE` writes it back (#14845).
my @cases =
    'only sub f() { }'             => 'only sub f { }',
    'only method m() { }'          => 'only method m { }',
    'our only sub f() { }'         => 'our only sub f { }',
    'sub f() { }'                  => 'sub f { }';

for @cases -> (:key($src), :value($deparsed)) {
    my $ast = $src.AST;
    is $ast.DEPARSE.trim, $deparsed, "DEPARSE of '$src'";
}

is 'only sub f() { }'.AST.statements[0].expression.multiness, 'only', 'multiness accessor says only';
is 'sub f() { }'.AST.statements[0].expression.multiness, '', 'a plain sub has no multiness';
is 'only submethod s() { }'.AST.DEPARSE.trim, 'only submethod s { }', 'submethod keeps only';

use MONKEY-SEE-NO-EVAL;
is EVAL('only sub g($x) { $x * 2 }; g(4)'.AST), 8, 'only sub still runs as EVAL($ast)';
is EVAL('only sub g($x) { $x * 3 }; g(4)'), 12, 'only sub still runs as EVAL(Str)';
ok 'only sub f() { }'.AST.raku.contains('multiness => "only"'), '.raku shows multiness';
