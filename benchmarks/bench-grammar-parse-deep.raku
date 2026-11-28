# Deep grammar-parse benchmark — exercises the O(n^2)-in-document-size pathology
# that the shallow `bench-grammar-parse.raku` does NOT surface. The same JSON-like
# grammar, but fed a document whose values are deeply nested arrays, so the
# per-subrule `RegexCaptures` sub-match tree grows large and the by-value capture
# threading (clone-per-DFS-step) pays O(tree) at every level.
#
# Sized so the current interpreter runs it in a few seconds (release). Compare
# mutsu vs raku: raku is ~O(n) here, mutsu is ~O(n^2), so the gap widens sharply
# with $PAIRS/$ARR. Use this for A/B when reworking the matcher (PLAN §5).
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

sub workload() {
    my $PAIRS = 60;
    my $ARR   = 10;
    my $inner = '[' ~ (1..$ARR).map({ "[$_,$_]" }).join(',') ~ ']';
    my $doc   = '{' ~ (1..$PAIRS).map({ "\"k$_\":$inner" }).join(',') ~ '}';

    my $m = JsonLike.parse($doc);
    die "parse failed" unless $m;
    "grammar-parse-deep: ok (doc {$doc.chars} chars, matched {$m.chars})";
}

# Warm timed section (convention: benchmarks/bench-regex-match.raku, docs/benchmarks.md):
# the workload runs twice untimed and a third time timed, and that third run is printed
# as `bench-section-seconds:`, so scripts/bench-ci.sh records a `@section` series free of
# interpreter startup. Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;
