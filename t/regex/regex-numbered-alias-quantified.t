# A numbered alias (`$N=`) inside a quantified `[ … ]` files one capture per
# iteration into slot N, which the quantifier folds into a list holding every
# iteration (#10792). The compiled engine used to number the inlined body
# against the whole level, so each iteration overwrote the previous one's slot
# and only the last iteration survived. Each case runs on the compiled engine
# and on the tree walk (`MUTSU_RX_VM=off`); both must print rakudo's values.
use Test;

my @cases =
    'one alias per iteration',
        'my $m = "12" ~~ / [ $0=(\d) ]+ /; say $m.list.elems, "|", $m[0].map({ .from ~ ".." ~ .to }).join(",")',
        '1|0..1,1..2',
    'alias over a non-capturing atom',
        'my $m = "abc" ~~ / [ $0=\w ]+ /; say $m.list.elems, "|", $m[0].map(~*).join(",")',
        '1|a,b,c',
    'a later group numbers on after the alias',
        'my $m = "a1b2" ~~ / [ $0=(\w) (\d) ]+ /; say $m[0].map(~*).join(","), "|", $m[1].map(~*).join(",")',
        'a,b|1,2',
    'frugal quantifier',
        'my $m = "123" ~~ / [ $0=(\d) ]+? 3 /; say $m[0].map(~*).join(",")',
        '1,2',
    'bounded quantifier',
        'my $m = "1234" ~~ / [ $0=(\d) ] ** 3 /; say $m[0].map(~*).join(",")',
        '1,2,3';

plan @cases / 3 * 2;

for @cases -> $name, $code, $expected {
    for <on off> -> $engine {
        my %env = %*ENV;
        %env<MUTSU_RX_VM> = $engine;
        my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
        is $proc.out.slurp(:close).trim, $expected, "$name (compiled engine $engine)";
        $proc.err.slurp(:close);
    }
}
