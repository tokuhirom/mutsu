use Test;

plan 3;

my regex demo ($param) {
    foo
    { $*RESULT = $param.raku }
    bar
}

my $*RESULT;
ok 'foobar' ~~ / <demo: { key => <v a l> }> /,
    'subrule accepts a block-literal argument';
is $*RESULT, '${:key($("v", "a", "l"))}',
    'embedded code receives the composite argument';
ok 'foobar' ~~ / <demo: { key => "value" }> /,
    'subrule also accepts a scalar Pair block argument';
