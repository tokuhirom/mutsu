use Test;

# A `$<name>=` alias on an atom quantified with a code count (`** { ... }`)
# names the WHOLE quantified span as one Match, exactly like the static
# `** N` / `+` forms -- it is not applied once per iteration (which used to
# leave `$<x>` a List of single-iteration Matches). Expected values are
# rakudo's.

plan 23;

# --- static count and code count agree -------------------------------------
{
    my $s = "aaaa" ~~ / $<x>=a ** 2 /;
    my $c = "aaaa" ~~ / $<x>=a ** {2} /;
    is $c.gist, $s.gist, '$<x>=a ** {2} gives the same Match as $<x>=a ** 2';
    is $c.gist, "｢aa｣\n x => ｢aa｣", 'the alias covers the whole span';
    isa-ok $c<x>, Match, '$<x> is a single Match, not a List';
    is $c<x>.from ~ ".." ~ $c<x>.to, '0..2', 'the alias span is the whole quantified run';
}

# --- ranges and greediness -------------------------------------------------
{
    is ("aaaa" ~~ / $<x>=a ** {1..3} /).gist, "｢aaa｣\n x => ｢aaa｣", 'greedy code range';
    is ("aaaa" ~~ / $<x>=a **? {1..3} /).gist, "｢a｣\n x => ｢a｣", 'frugal code range';
    is ("aaab" ~~ / $<x>=a ** {1..3} b /).gist, "｢aaab｣\n x => ｢aaa｣",
        'the alias is followed by the rest of the pattern';
    is ("aaab" ~~ / $<x>=a ** {1..3}: b /).gist, "｢aaab｣\n x => ｢aaa｣",
        'a ratcheted code count';
}

# --- a zero count captures an empty Match ----------------------------------
{
    is ("aaaa" ~~ / $<x>=a ** {0} /).gist, "｢｣\n x => ｢｣", 'zero iterations: empty span';
    is ("b" ~~ / $<x>=a ** {0} b /).gist, "｢b｣\n x => ｢｣", 'zero iterations before more pattern';
}

# --- the count may read an earlier capture ----------------------------------
{
    is ("3aaaa" ~~ / $<n>=\d $<x>=a ** {$<n>} /).gist, "｢3aaa｣\n n => ｢3｣\n x => ｢aaa｣",
        'the count code sees the capture made before the alias';
    is ("2aaaa" ~~ / $<n>=\d $<x>=a ** {+$<n>} /).gist, "｢2aa｣\n n => ｢2｣\n x => ｢aa｣",
        'the count code numifies an earlier capture';
}

# --- an aliased capture GROUP stays per iteration --------------------------
{
    is ("aaaa" ~~ / $<x>=(a) ** {2} /).gist, ("aaaa" ~~ / $<x>=(a) ** 2 /).gist,
        'an aliased capture group with a code count matches the static count';
    is ("aaaa" ~~ / $<x>=(a) ** {2} /).gist, "｢aa｣\n x => ｢a｣\n x => ｢a｣",
        'one Match per group iteration';
    is ("aaaa" ~~ / $<x>=(a) ** {2} /)<x>.elems, 2, 'two group Matches';
}

# --- array and numbered aliases --------------------------------------------
{
    is ("aaaa" ~~ / @<x>=a ** {2} /).gist, ("aaaa" ~~ / @<x>=a ** 2 /).gist,
        '@<x>= with a code count matches the static count';
    is ("aaaa" ~~ / $0=a ** {2} /).gist, ("aaaa" ~~ / $0=a ** 2 /).gist,
        '$0= with a code count matches the static count';
    is ("aaaa" ~~ / $0=a ** {2} /).gist, "｢aa｣\n 0 => ｢aa｣", 'the numbered alias covers the span';
}

# --- a separator and sigspace ----------------------------------------------
{
    is ("a,a,a" ~~ / $<x>=a ** {2} % ',' /).gist, "｢a,a｣\n x => ｢a,a｣",
        'a separated code count names the whole separated span';
    is ("a a a" ~~ / :s $<x>=a ** {2} /).gist, ("a a a" ~~ / :s $<x>=a ** 2 /).gist,
        'sigspace code count matches the static count';
}

# --- inside a group, and a failed match -------------------------------------
{
    is ("aaaabb" ~~ / [ $<x>=a ** {2} ] $<y>=b ** {2} /).gist, "｢aabb｣\n x => ｢aa｣\n y => ｢bb｣",
        'two code-count aliases, one inside a group';
    nok ("a" ~~ / $<x>=a ** {2} /).Bool, 'too few iterations: no match';
    is ("a" ~~ / $<x>=a ** {2} /).gist, 'Nil', 'and the result is Nil';
}
