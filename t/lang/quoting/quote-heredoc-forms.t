use Test;

plan 12;

# `to` is a fused adverb: qto/qqto/Qto are q:to/qq:to/Q:to.
my $who = 'world';

is qto/END/, "plain\n", 'qto is q:to';
plain
END

is qqto/END/, "hello world\n", 'qqto is qq:to';
hello $who
END

is Qto/END/, 'raw $who' ~ "\n", 'Qto is Q:to';
raw $who
END

# A heredoc still takes its adverbs the long way round.
is q:to/END/, "plain\n", 'q:to still works';
plain
END

# Whitespace -- including a newline -- may separate a quote word from its delimiter.
is q <spaced>, 'spaced', 'q accepts a space before the delimiter';
is q /slashed/, 'slashed', 'q accepts a space before a symmetric delimiter';
is qq <interp $who>, 'interp world', 'qq accepts a space before the delimiter';

my $across = q
<first
second
>;
is $across, "first\nsecond\n", 'q accepts a newline before the delimiter';

# `q` is still a pair key, not a quote, in front of a fat arrow.
my %pair = (q => 1);
is %pair<q>, 1, 'q => stays a pair key';

# The heredoc body starts after the line the *parser* finishes, so a newline that a
# string literal already swallowed does not open it.
my @parts = q:to/END/, "one
two";
body
END
is @parts.join('|'), "body\n|one\ntwo", 'heredoc body starts below the whole statement';

# A selective adverb interpolates only what it names: :c takes closures, not $vars.
my $cents = 18;
is q:to:c/END/.chop, '$d and 18 c.', ':c interpolates closures but not variables';
$d and {$cents} c.
END

is q:c/$d and {$cents}/, '$d and 18', ':c on a non-heredoc q is unchanged';
