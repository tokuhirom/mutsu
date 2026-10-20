use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `repeat while/until C -> $p { }` round-trips through RakuAST with its pointy
# parameter: the body is a `PointyBlock` whose signature declares `$p`, and
# lowering it back declares the parameter again (it used to be dropped, which
# left `$p` undeclared in the lowered program). Passes under mutsu and raku.

plan 8;

is EVAL(Q[my $i = 0; my $u = 0; repeat while $i < 3 -> $p { $u++ unless $p.defined; $i++ }; "$i/$u"].AST),
    '3/1', 'repeat while -> $p: the parameter is declared and starts undefined';

is EVAL(Q[my $i = 0; my $u = 0; repeat until $i >= 2 -> $p { $u++ unless $p.defined; $i++ }; "$i/$u"].AST),
    '2/1', 'repeat until -> $p: the parameter is declared and starts undefined';

is EVAL(Q[my $i = 0; my $d = 0; repeat while $i < 3 -> $p { $d++ if $p; $i++ }; $d].AST),
    2, 'repeat while -> $p: later passes see the true condition';

my $ast = Q[my $i = 0; repeat until $i >= 2 -> $p { $i++ }].AST.raku;
ok $ast.contains('RakuAST::PointyBlock.new(') && $ast.contains('name => "\$p"'),
    'the loop body is a PointyBlock whose signature declares $p';

is EVAL(Q[my @v; my $i = 0; repeat while 3 - $i -> $p { @v.push($p.raku); $i++ }; @v.join('|')].AST),
    'Mu|2|1', 'round-tripped while preserves Mu and the numeric condition';

is EVAL(Q[my @v; my $i = 0; repeat until $i >= 3 -> $p { @v.push($p.raku); $i++ }; @v.join('|')].AST),
    'Mu|Bool::False|Bool::False', 'round-tripped until supplies the unnegated condition';

my $written = Q[my $i = 0; repeat while 3 - $i -> $p { $i++ }].AST.statements[1].condition.raku;
ok !$written.contains('Assign'), 'the source condition has no synthetic parameter assignment';

is EVAL(Q[my $i = 0; my $calls = 0; sub cond { $calls++; 3 - $i }; repeat while cond() -> $p { $i++ }; $calls].AST),
    3, 'round-trip evaluates the condition once between passes';
