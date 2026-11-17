use Test;

# A token / rule / regex closes over the scope its grammar (or the `my regex`
# itself) is declared in (#17872): its code blocks, assertions and
# interpolated variables read the declaring routine's or block's lexicals,
# even after that scope has returned, and a write reaches them. Expectations
# were measured against rakudo 2026.09.

plan 10;

sub mk() { my $m = "q"; my grammar K { token TOP { x <?{ $m eq "q" }> } }; K }
ok mk().parse("x"), 'a `my grammar` in a sub reads the sub\'s lexical in an assertion';

sub mk-our() { my $m = "q"; grammar K3 { token TOP { x <?{ $m eq "q" }> } }; K3 }
ok mk-our().parse("x"), 'so does a package-scoped grammar declared in a sub';

{ my $m = "q"; grammar K6 { token TOP { x <?{ $m eq "q" }> } }; }
ok K6.parse("x"), 'and a grammar declared in a bare block';

sub mk-atom() { my $m = "q"; my grammar KA { token TOP { x $m } }; KA }
ok mk-atom().parse("xq"), 'an interpolated variable of the declaring scope';

sub mk-sub() { my $m = "q"; my grammar KS { token TOP { <t>+ }; token t { $m } }; KS }
ok mk-sub().parse("qqq"), 'in a subrule too';

{
    sub mk-write() { my $n = 0; my grammar KW { token TOP { [x { $n++ }]+ } }; (KW, -> { $n }) }
    my ($g, $get) = mk-write();
    $g.parse("xxx");
    is $get(), 3, 'a code block writes the declaring scope\'s variable';
}

sub mk-regex() { my $m = "q"; my regex rx-a { x <?{ $m eq "q" }> }; &rx-a }
{
    my &r = mk-regex();
    ok "x" ~~ /<&r>/, 'a `my regex` returned from a sub keeps its scope';
}

sub mk-regex-atom() { my $m = "q"; my regex rx-b { x $m }; &rx-b }
{
    my &r = mk-regex-atom();
    ok "xq" ~~ /<&r>/, '... for an interpolated variable';
}

# Each entry of the declaring sub closes over that entry's variables.
sub mk-n($v) { my $m = $v; my grammar KN { token TOP { $m } }; KN }
ok mk-n("a").parse("a"), 'first entry';
ok mk-n("b").parse("b"), 'second entry sees its own value';
