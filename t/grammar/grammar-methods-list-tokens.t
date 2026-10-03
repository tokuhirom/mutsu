use Test;

# A grammar's token/rule/regex declarations are Regex methods: `.^methods`
# lists them, so `G.^methods.grep({ .WHAT ~~ Regex })` finds and can wrap
# them (Grammar::Extractor).

plan 5;

grammar G {
    token TOP  { <a>+ }
    token a    { 'a' }
    rule  pair { <a> <a> }
    method helper { 1 }
}
grammar H is G {
    token b { 'b' }
}

is-deeply G.^methods.grep({ .WHAT ~~ Regex }).map(*.name).sort.List,
    <TOP a pair>, 'tokens and rules are listed as Regex methods';
ok G.^methods.first(*.name eq 'helper'), 'ordinary methods are still listed';
is-deeply H.^methods(:local).grep({ .WHAT ~~ Regex }).map(*.name).List,
    ('b',), ':local lists only the grammar\'s own tokens';
ok H.^methods.grep({ .WHAT ~~ Regex }).map(*.name).grep('a'),
    'inherited tokens are listed without :local';

my $calls = 0;
for G.^methods.grep({ .WHAT ~~ Regex }) -> &r {
    &r.wrap: my sub (|c) { $calls++; callsame }
}
G.parse('aaa');
ok $calls >= 4, 'the listed methods can be wrapped and the wrappers run';
