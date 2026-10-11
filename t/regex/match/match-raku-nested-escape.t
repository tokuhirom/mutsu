use Test;

# Match.raku renders every nested node with the whole `orig`, escaped. It is
# written into one buffer, so a deeply nested match costs the length of its
# text, not that length times the depth.

plan 5;

"a\"b" ~~ /(a) (<[\"]>) (b)/;
is $/.raku,
    'Match.new(:orig("a\"b"), :from(0), :pos(3), :list((Match.new(:orig("a\"b"), :from(0), :pos(1)), Match.new(:orig("a\"b"), :from(1), :pos(2)), Match.new(:orig("a\"b"), :from(2), :pos(3)))))',
    'quotes in orig are escaped at every node';

"xy" ~~ /(x) (y)/;
is $/.raku,
    'Match.new(:orig("xy"), :from(0), :pos(2), :list((Match.new(:orig("xy"), :from(0), :pos(1)), Match.new(:orig("xy"), :from(1), :pos(2)))))',
    'positional captures are comma-joined';

"x\ty" ~~ /x \t y/;
is $/.raku, 'Match.new(:orig("x\ty"), :from(0), :pos(3))', 'tab is escaped';

"a\\b" ~~ /a \\ b/;
is $/.raku, 'Match.new(:orig("a\\\\b"), :from(0), :pos(3))', 'backslash is escaped';

grammar Deep { token TOP { . <TOP> | \d+ } }
my $text = "." x 200 ~ "42";
my $m = Deep.parse($text);
my $raku = $m.raku;
is $raku.chars > 200 * 200, True, 'a 200-deep match renders every node with its orig';

done-testing;
