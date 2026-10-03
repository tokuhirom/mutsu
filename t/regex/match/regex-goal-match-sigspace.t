use v6;
use Test;

plan 7;

# The `~` goal construct in a `rule` (TOML::Thumb's `table` and
# `inline-array` rules).

# Whitespace written after the inner atom separates the whole construct from
# what follows, so it is matched after the closer.
grammar H {
    token ws { \s* }
    rule table { '[' ~ ']' <key> <kv>* }
    token kv { <key> '=' \d }
    token key { <[a..z]>+ }
}
ok H.parse("[a]b=1", :rule<table>), 'no whitespace';
ok H.parse("[a] b=1", :rule<table>), 'space after the closer';
ok H.parse("[a]\nb=1", :rule<table>), 'newline after the closer';

grammar T {
    token ws { \s* }
    rule table { '[' ~ ']' <key> 'x' }
    token key { <[a..z]>+ }
}
ok T.parse("[a] x", :rule<table>), 'literal after the construct';

# The whitespace inside the brackets is the grammar's own `ws` (here: comments
# count as whitespace).
grammar C {
    token ws { [ \s | '#' \N* ]* }
    rule TOP { '[' ~ ']' <v>* %% ',' }
    token v { \d+ }
}
ok C.parse("[ #c\n 1, 2 ]"), 'grammar ws override after the opener';
ok C.parse("[ 1, 2, #x\n]"), 'grammar ws override before the closer';

grammar P { rule TOP { '(' ~ ')' <k>+ % ',' 'z' }; token k { <[a..z]> } }
ok P.parse("(a,b) z"), 'separated inner atom then a trailing literal';
