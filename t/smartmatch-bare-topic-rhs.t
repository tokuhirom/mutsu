use v6;
use Test;

# The RHS of a smartmatch normally runs with `$_` already bound to the LHS --
# `$x ~~ s///` has to topicalize `$x` so the substitution has something to write
# through. That made a BARE `$_` on the right evaluate to the LHS instead of to
# the enclosing topic, so
#
#     for (/a/,) { say ("ab" ~~ $_).raku }
#
# really evaluated `"ab" ~~ "ab"` -- a string-vs-string match that is correctly
# True, but which never ran the regex the topic actually held. The `Match` (and
# `$/`) were lost, and `.map` over a list of regexes answered `(True True True)`
# where raku answers with the three Match objects.
#
# rakudo special-cases exactly this SPELLING, not "an RHS that reads the topic":
# `("ab" ~~ ($_))` in parentheses really does answer True there, and so do
# `$_[0]`, `$_<k>` and `f($_)`. Test 2 pins that deliberately.
#
# Every expectation below was measured against rakudo 2026.07.

plan 19;

# --- the ticket's repro ------------------------------------------------
for (/a/,) {
    my $m = "ab" ~~ $_;
    isa-ok $m, Match, 'a bare $_ RHS holding a regex yields the Match';
    is $m.Str, 'a', 'whose contents are the matched text';
}

# The doc block it came from, one layer up.
my @regex-check = (/<alnum>/, /<alpha>/, /<punct>/);
my @out = @regex-check.map({ "33af" ~~ $_ });
is @out[0].Str, '3', '.map over a list of regexes yields Matches (alnum)';
is @out[1].Str, 'a', '... (alpha)';
nok @out[2].defined, '... and Nil for the one that does not match';

# --- the parenthesised form is NOT the same, in raku either ------------
for (/a/,) {
    my $p = "ab" ~~ ($_);
    is $p.^name, 'Bool', 'a PARENTHESISED ($_) still topicalizes, as in raku';
    ok $p, 'and is True, being "ab" ~~ "ab"';
}

# Neither are the other topic-reading shapes.
for ((/a/,),) { is ("ab" ~~ $_[0]).^name, 'Bool', 'an indexed topic still topicalizes' }
sub id($x) { $x }
for (/a/,) { is ("ab" ~~ id($_)).^name, 'Bool', 'and so does a call taking the topic' }

# --- $/ must be populated, exactly as a direct match populates it ------
for (/a/,) {
    "ab" ~~ $_;
    is $/.Str, 'a', 'a bare $_ RHS sets $/ like any other regex match';
}

# --- the other ACCEPTS families reachable through a bare $_ ------------
# Each answers what its own `ACCEPTS` returns, which is what the direct
# spelling already answered.
for ("ab",)        { is ("ab" ~~ $_).^name, 'Bool', 'a Str topic answers Bool' }
for (Int,)         { ok  3 ~~ $_,                   'a type-object topic type-checks' }
for (1..5,)        { ok  3 ~~ $_,                   'a Range topic range-checks' }
for (set(<a b>),)  { nok "a" ~~ $_,                 'a Set topic uses Any.ACCEPTS, so it is False' }
for ({ $_ > 2 },)  { ok  3 ~~ $_,                   'a Block topic is called with the LHS' }
for (any(1,2,3),)  { ok  2 ~~ $_,                   'a Junction topic threads' }

# --- negation, and the nested case -------------------------------------
for (/a/,) { nok "ab" !~~ $_, 'a negated bare $_ RHS negates the Match' }
for (/a/,) { for (/b/,) { is ("ab" ~~ $_).Str, 'b', 'a nested topic wins over the outer one' } }

# --- the topicalization the change must NOT cost -----------------------
# `$x ~~ s///` still binds `$_` to `$x` so the substitution writes through.
{
    my $s = "abc";
    $s ~~ s/b/X/;
    is $s, 'aXc', 'an s/// RHS still topicalizes its LHS variable';
}
