use Test;

plan 3;

grammar G {
    has $.v;
    token TOP { <t> }
    token t { a { self.acc } }
    method acc { $!v = 'viaself' }
}

lives-ok { G.parse("a") }, '`self` in a token code block is available';
is G.parse("a")<t>.v.raku, 'Any', 'the block ran on a cursor, not the Match attribute';

grammar H {
    token TOP { a { $*seen = self.^name } b }
}
my $*seen = '';
H.parse("ab");
is $*seen, 'H', '`self` in a code block is an instance of the grammar';
