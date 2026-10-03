# ADR-0135 Slice E: a left-recursive `<subrule>` call is evaluated by the
# growing-seed loop (`subrule_seed_ends`) directly from the compiled regex
# engine, not through the tree walk's eager producer. Rakudo has no answer to
# compare with (it loops forever on left recursion), so these pin the values
# mutsu's growing-seed evaluation has always produced, now from the compiled
# engine.
use Test;

plan 7;

grammar Expr {
    token TOP  { <expr> }
    token expr { <expr> '+' <term> | <term> }
    token term { \d+ }
}
my $m = Expr.parse('1+2+3');
is ~$m<expr>, '1+2+3', 'a left-recursive rule grows to the whole input';
is ~$m<expr><expr>, '1+2', 'its seed is the shorter match';
is ~$m<expr><term>, '3', 'the last term';

grammar Args {
    token TOP { <e(1)> }
    token e($n) { <e($n)> 'b' | 'a' }
}
is ~Args.parse('abb'), 'abb', 'a left-recursive call with arguments';

{
    my @log;
    grammar Code {
        token TOP { <e> '!' }
        token e { <e> 'x' { @log.push: $/.to } | 'x' }
    }
    ok Code.parse('xxx!'), 'a left-recursive rule with a code block parses';
    is @log.join(','), '2,3', 'the block runs once per grown seed';
}

{
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e',
        q:to/CODE/, :out, :err, :%env);
        grammar G { token TOP { <e> }; token e { <e> '+' \d | \d } }
        say ~G.parse("1+2")
        CODE
    $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('regex-walk:')) // '';
    like $line, /'walked=0 () bridged=0 ()'/, 'left recursion no longer bridges to the walk';
}
