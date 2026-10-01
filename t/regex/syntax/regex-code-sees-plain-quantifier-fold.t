# Code inside an iteration of an unseparated quantifier (`*`, `+`, `** n..m`)
# sees the quantifier's capture slots folded, as rakudo's one cursor does: a
# capture group under the quantifier is one slot from the first iteration on,
# holding every iteration so far (#10597). Each case runs on the compiled
# engine and on the tree walk (`MUTSU_RX_VM=off`); both must print rakudo's
# values (`MUTSU_RX_DIFF=1` compares the two engines' views as well).
use Test;

my $prelude = q:to/END/;
    my @log;
    sub show($m) {
        $m.list.map({ $_ ~~ Positional ?? '[' ~ .map(~*).join(',') ~ ']' !! ($_ // 'Nil').Str }).join('|')
    }
    END

my @cases =
    'plain +',
        '"123" ~~ / [ (\d) { @log.push: show($/) } ]+ /',
        '[1] ; [1,2] ; [1,2,3]',
    'plain + nested in a separated quantifier',
        '"12;3" ~~ / [ [ (\d) { @log.push: show($/) } ]+ ] +% \';\' /',
        '[1] ; [1,2] ; [1,2,3]',
    'separated quantifier nested in a plain +',
        '"1.2 3.4" ~~ / [ [ (\d) { @log.push: show($/) } ] +% \'.\' \' \'? ]+ /',
        '[1] ; [1,2] ; [1,2,3] ; [1,2,3,4]',
    'plain + nested in a plain +',
        '"12 34" ~~ / [ [ (\d) { @log.push: show($/) } ]+ \' \'? ]+ /',
        '[1] ; [1,2] ; [1,2,3] ; [1,2,3,4]',
    'counted quantifier',
        '"1234" ~~ / [ (\d) { @log.push: show($/) } ] ** 3 /',
        '[1] ; [1,2] ; [1,2,3]',
    'frugal quantifier',
        '"123" ~~ / [ (\d) { @log.push: show($/) } ]+? 3 /',
        '[1] ; [1,2]',
    'a capture before the quantifier keeps its slot',
        '"ab12" ~~ / (a) (b) [ (\d) { @log.push: show($/) } ]* /',
        'a|b|[1] ; a|b|[1,2]',
    'an unmatched (x)? adds nothing to its slot',
        '"a123" ~~ / (a) [ (\d) (\d)? { @log.push: show($/) } ]+ /',
        'a|[1]|[2] ; a|[1,3]|[2]',
    'a [ … ] inside the iteration folds into its slots',
        '"1x2x" ~~ / [ (\d) [ (x) { @log.push: show($/) } ] ]+ /',
        '[1]|[x] ; [1,2]|[x,x]',
    'the same inside a separated quantifier',
        '"1x,2x" ~~ / [ (\d) [ (x) { @log.push: show($/) } ] ] +% \',\' /',
        '[1]|[x] ; [1,2]|[x,x]',
    'a [ … ] after a capture in a separated iteration (#10599)',
        '"xa,xb" ~~ / [ (x) [ (\w) { @log.push: show($/) } ] ] +% \',\' /',
        '[x]|[a] ; [x,x]|[a,b]',
    'a code assertion',
        '"123" ~~ / [ (\d) <?{ @log.push: show($/); $/[0].elems < 3 }> ]+ /',
        '[1] ; [1,2] ; [1,2,3]',
    'alternation branches',
        '"ab1" ~~ / [ (a) { @log.push: show($/) } | (b) { @log.push: show($/) } | (\d) ]+ /',
        '[a] ; [a,b]',
    'a backreference to a capture before the quantifier',
        '"a1a2a" ~~ / (a) [ (\d) $0 { @log.push: show($/) } ]+ /',
        'a|[1] ; a|[1,2]';

for @cases -> $name, $match, $expected {
    my $code = $prelude ~ $match ~ '; say @log.join(" ; ")';
    for <on off> -> $engine {
        my %env = %*ENV;
        %env<MUTSU_RX_VM> = $engine;
        my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
        is $proc.out.slurp(:close).trim, $expected, "$name (compiled engine $engine)";
        $proc.err.slurp(:close);
    }
}

is ("x1xx" ~~ /[ (\d)? x ]+/)[0].map(~*).join(','), '1',
    'an iteration whose (x)? did not match adds nothing to the folded list';

done-testing;
