# Grammar parsing benchmark: a JSON-like grammar exercising the constructs
# real-world grammars lean on — proto token dispatch (LTM), rules (ratchet +
# sigspace), separated quantifiers (`* %`), goal-post `'{' ~ '}'` pairing,
# and an escape-handling string token with an alternation quantifier.
# This is the abstracted form of the roast S04-exceptions/exceptions-alternatives.t
# JSON::Tiny pathology: `rule pairlist { <pair> * % \, }` used to backtrack
# exponentially (~4x per extra pair) before ratchet sep-quantifiers became
# possessive.
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

my $doc = q:to/END/.chomp;
{"name":"bench","version":"1.0.3","enabled":true,"retries":null,
 "message":"line one\nline two\ttabbed A done",
 "limits":{"cpu":4,"mem":2048,"burst":-1.5e2},
 "tags":["a","b","c"],
 "matrix":[[1,2],[3,4]]}
END

my $ok = 0;
for ^3 {
    my $m = JsonLike.parse($doc);
    die "parse failed" unless $m;
    $ok++ if $m<value><object><pairlist><pair>[0]<value><string><str>[0].Str eq 'bench';
}
say "grammar-parse: $ok/3 ok";
