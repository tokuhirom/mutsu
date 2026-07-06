use v6;
use Test;

# `$str ~~ m:g/.../` (and the other multi-match adverb paths :x/:ov/:ex/:nth)
# builds a list of Match objects. Each match's `.orig` must report the whole
# subject string, matching the single-match path and `.match(:g)`. The
# `~~ m:g` path previously passed `None` as the origin, leaving `.orig` empty
# (and rendering as `:orig("")` in `.raku`).

plan 8;

{
    my @m = "a1b2" ~~ m:g/(\w)/;
    is @m[0].orig, "a1b2", 'm:g match .orig reports the subject string (1)';
    is @m[1].orig, "a1b2", 'm:g match .orig reports the subject string (2)';
}

{
    # nested positional captures also carry orig
    my @m = "a1b2" ~~ m:g/(\w)(\d)/;
    is @m[0][0].orig, "a1b2", 'm:g nested capture .orig reports the subject string';
    is @m[0].Str, "a1", 'm:g match .Str still correct';
}

{
    # :ov (overlap) path
    my @m = "aaa" ~~ m:ov/a+/;
    is @m[0].orig, "aaa", 'm:ov match .orig reports the subject string';
}

{
    # :x(N) path
    my @m = "aXbXc" ~~ m:x(2)/\w/;
    is @m[1].orig, "aXbXc", 'm:x(N) match .orig reports the subject string';
}

{
    # :nth multi path
    my @m = "a1b2c3" ~~ m:nth(1,2)/(\w)/;
    is @m[0].orig, "a1b2c3", 'm:nth multi match .orig reports the subject string';
}

{
    # the single-match path is unaffected
    is ("abc" ~~ /b/).orig, "abc", 'single match .orig unaffected';
}
