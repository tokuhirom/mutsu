use v6;
use Test;

# A `qq:to` / `q:to` heredoc may have whitespace — including a newline — between
# the `:to` adverb and the quoted delimiter, e.g. when the delimiter sits on the
# line below in list-op / parenthesised argument position:
#
#     say qq:to
#     '====='
#     body
#     =====
#
# Real dists (App::pixelpick's USAGE block) write their heredocs this way.
# Regression: mutsu's delimiter scanner only trimmed spaces, so a newline before
# the delimiter made it reject the heredoc and mis-parse `qq:to` as a routine
# call ("Undeclared routine: qq:to").

plan 4;

my $a = (qq:to
"END"
hello world
END
);
is $a.chomp, 'hello world', 'qq:to with a newline before a "-delimiter';

my $b = (q:to
'====='
plain text
=====
);
is $b.chomp, 'plain text', "q:to with a newline before a '-delimiter";

# Interpolation still works in the newline form.
my $who = 'raku';
my $c = (qq:to
"STOP"
hi $who
STOP
);
is $c.chomp, 'hi raku', 'interpolation works in the newline-delimiter form';

# The classic inline form is unaffected.
my $d = qq:to/FIN/;
inline body
FIN
is $d.chomp, 'inline body', 'inline qq:to/FIN/ still works';
