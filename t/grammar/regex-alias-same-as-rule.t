use Test;

# `$<b>=<b>` aliases a rule call to the rule's own name: one capture, not two
# beside each other. A grammar method that answers a Match hands on its `.ast`.

plan 6;

grammar G {
    token b { \w+ }
    token t { $<b>=<b> }
    token u { <b> }
    method meth() {
        my $m = Match.new(:orig(self.orig), :from(self.pos), :to(self.pos + 2));
        $m.make('made-by-meth');
        $m
    }
    token v { $<m>=<meth> }
    token w { <meth> }
}

my $t = G.subparse('abc', :rule<t>);
isa-ok $t<b>, Match, 'the alias of a rule to its own name is one Match, not a list';
is ~$t<b>, 'abc', 'and it spans the rule';
is G.subparse('abc', :rule<t>).hash.keys.sort.List, ('b',), 'the capture is filed once';
is-deeply G.subparse('abc', :rule<t>).raku, G.subparse('abc', :rule<u>).raku,
    'the aliased form builds what the plain call builds';

my $v = G.subparse('abc', :rule<v>);
is $v<m>.ast, 'made-by-meth', 'the Match a method answers keeps its .ast under an alias';
is G.subparse('abc', :rule<w>)<meth>.ast, 'made-by-meth', 'and under its own name';
