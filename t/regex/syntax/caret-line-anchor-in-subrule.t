use Test;

# The `^^` (start-of-line) anchor must respect the position in the *original*
# parse text, not the start of the slice a subrule is matched against. A subrule
# (`<foo>`) is matched against `chars[pos..]` in a sub-interpreter; previously
# `^^` at the subrule's entry always succeeded (slice position 0 looked like a
# line start) even when the subrule was entered mid-line. That made a
# non-standalone tag wrongly match a standalone line rule and swallow the
# newline that ended the previous content line (seen in Template::Mustache's
# `hunk`/`linetag` grammar — section iterations ran together on one line).

plan 6;

# `^^` at a subrule called mid-line (preceded by a non-newline) must FAIL.
grammar MidLine {
    token TOP { 'H' <close> }
    token close { ^^ 'X' }
}
nok MidLine.parse('HX').defined,
    '^^ in a subrule entered mid-line does not match';

# `^^` at a subrule called right after a newline must MATCH.
grammar AfterNl {
    token TOP { 'a' \n <close> }
    token close { ^^ 'X' }
}
ok AfterNl.parse("a\nX").defined,
    '^^ in a subrule entered after a newline matches';

# `^^` in a subrule at the true start of the parse text still matches.
grammar AtStart {
    token TOP { <s> 'rest' }
    token s { ^^ 'go' }
}
ok AtStart.parse('gorest').defined,
    '^^ in a subrule at the true text start matches';

# The motivating case (mirrors Template::Mustache's grammar): a standalone
# line-tag rule (`^^ ... <tag> ... \n`) must not consume the newline ending the
# *previous*, non-standalone content line.
grammar Hunks {
    regex TOP { ^ <hunk>* (.*) $ }
    regex hunk { (.*?) [ <linetag> | <tag> ] }
    token linetag { ^^ (\h*) <tag> \h* [\n | $] }
    token tag { '{' <( <[\w/]>+ )> '}' }
}
class Collect {
    method tag($/)     { make ~$/ }
    method linetag($/) { make 'LINE:' ~ $<tag>.made }
    method hunk($/) {
        my @x;
        @x.push('T<' ~ ~$0 ~ '>') if ~$0;
        for $<linetag tag>.grep(*.defined)>>.made -> $m { @x.push($m) }
        make @x.Slip;
    }
    method TOP($/) { make $<hunk>>>.made.flat.join('|') ~ '|tail<' ~ ~$0 ~ '>' }
}

# '{/x}' is NOT standalone (preceded by 'H'), so the newline before the
# standalone '{/end}' survives as its own text hunk.
my $nl = "\n";
is Hunks.parse("a\{x}H\{/x}\n\{/end}body", :actions(Collect)).made,
    "T<a>|x|T<H>|/x|T<$nl>|/end|tail<body>",
    'a non-standalone close tag leaves the trailing newline for the next hunk';

# Regression guard: without the nested mid-line tag the result is unchanged.
is Hunks.parse("a\{x}\n\{/end}body", :actions(Collect)).made,
    "T<a>|x|T<$nl>|/end|tail<body>",
    'trailing newline preserved (no nested tag) — unchanged';

# A standalone tag on its own line still consumes its whole line (newline gone).
is Hunks.parse("\{open}\nbody", :actions(Collect)).made,
    'LINE:open|tail<body>',
    'a standalone tag consumes its own line including the newline';
