# Grammar parsing over a BIG document: growth rate, not per-parse setup.
#
# `bench-grammar-parse.raku` parses a ~250-byte document three times and
# `bench-grammar-parse-deep.raku` one ~200-byte nested document once; both now
# run in ~16 ms on CI, which is a few times the 8 ms startup. At that size the
# measurement is dominated by fixed per-parse cost, so neither can see the thing
# that actually matters about a grammar engine: how its cost grows with the
# length of the input. (The header of bench-grammar-parse-deep.raku still claims
# it is "sized so the current interpreter runs it in a few seconds" -- that was
# true when it was written and the engine has since gained two orders of
# magnitude on it. It is kept at its size so its history stays continuous; this
# file is the re-sized one.)
#
# Same JSON-like grammar, ~13 KB of document: 320 pairs whose values are nested
# arrays. Measured locally (release, 2026-09-13) against rakudo 2026.07, scaling
# the pair count:
#
#     5 pairs    22 ms vs 716 ms      20 pairs    23 ms vs 471 ms
#    80 pairs    50 ms vs 462 ms     320 pairs   156 ms vs 595 ms
#
# i.e. linear from 80 pairs up (4x the document for 3.6x the time), which is the
# property to defend: any change that reintroduces a per-node cost proportional
# to the whole match tree turns this file quadratic long before the two small
# grammar files notice anything.
grammar JsonLike {
    token TOP       { \s* <value> \s* }
    rule object     { '{' ~ '}' <pairlist>     }
    rule pairlist   { <pair> * % \,            }
    rule pair       { <string> ':' <value>     }
    rule array      { '[' ~ ']' <arraylist>    }
    rule arraylist  {  <value> * % [ \, ]        }

    proto token value {*};
    token value:sym<number> {
        '-'?
        [ 0 | <[1..9]> <[0..9]>* ]
        [ \. <[0..9]>+ ]?
        [ <[eE]> [\+|\-]? <[0..9]>+ ]?
    }
    token value:sym<true>    { <sym>    };
    token value:sym<false>   { <sym>    };
    token value:sym<null>    { <sym>    };
    token value:sym<object>  { <object> };
    token value:sym<array>   { <array>  };
    token value:sym<string>  { <string> }

    token string { ('"') ~ \" [ <str> | \\ <str=.str_escape> ]* }
    token str { <-["\\\t\x[0A]]>+ }
    token str_escape { <["\\/bfnrt]> | 'u' <utf16_codepoint>+ % '\u' }
    token utf16_codepoint { <.xdigit>**4 }
}

my $PAIRS = 320;
my $inner = '[' ~ (1..4).map({ "[$_,$_]" }).join(',') ~ ']';
my $doc   = '{' ~ (1..$PAIRS).map({ "\"k$_\":$inner" }).join(',') ~ '}';

my $m = JsonLike.parse($doc);
die "parse failed" unless $m;
say "grammar-parse-big: ok (doc {$doc.chars} chars, matched {$m.chars})";
