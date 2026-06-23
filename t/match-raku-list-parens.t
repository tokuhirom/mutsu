use Test;

plan 6;

# A Match with positional captures renders `.raku` with a balanced
# `:list(( ... ))` — an outer paren for the adverb and an inner paren for
# the captured List. The closing paren of the adverb must not be dropped.
sub balanced($s) { $s.comb('(').elems == $s.comb(')').elems }

"hello world" ~~ /(\w+) \s+ (\w+)/;
my $two = $/.raku;
ok balanced($two), 'two-capture Match .raku has balanced parens';
is $two,
   'Match.new(:orig("hello world"), :from(0), :pos(11), :list((Match.new(:orig("hello world"), :from(0), :pos(5)), Match.new(:orig("hello world"), :from(6), :pos(11)))))',
   'two-capture Match .raku matches Rakudo';

"ab" ~~ /(.)/;
my $one = $/.raku;
ok balanced($one), 'single-capture Match .raku has balanced parens';
is $one,
   'Match.new(:orig("ab"), :from(0), :pos(1), :list((Match.new(:orig("ab"), :from(0), :pos(1)),)))',
   'single-capture Match .raku has the trailing comma and balanced parens';

# .raku output must EVAL back to an equivalent Match.
"xy" ~~ /(.)(.)/;
my $m = $/.raku.EVAL;
is $m.list.elems, 2, 'Match .raku round-trips via EVAL (positional count)';
is ~$m, 'xy', 'round-tripped Match stringifies to the matched text';
