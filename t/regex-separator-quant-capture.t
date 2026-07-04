use Test;

# A capturing group under a `**N`/`**N..M` quantifier WITH a `%`/`%%` separator
# folds every repetition into a single positional group (index 0), matching
# Raku's Match layout — NOT one spurious positional slot per repetition. This
# was broken for the exact/range `**` forms because the string-based LTM
# expansion renumbered the captures (`(\d)**4 % '.'` yielded groups 0,1,2,3);
# the native separator-quantifier matcher folds them correctly.

plan 10;

# --- exact count ---
{
    my $m = "a1b" ~~ / (\w) ** 2 % \d /;
    ok $m, 'exact **2 % sep matches';
    is $m[0].elems, 2, 'exact **2 %: group 0 folds both repetitions';
    is $m[0].map(*.Str).join(','), 'a,b', 'exact **2 %: folded captures are correct';
}

# --- range count ---
{
    my $m = "a1b2c" ~~ / (\w) ** 1..3 % \d /;
    is $m[0].elems, 3, 'range **1..3 %: group 0 folds all three repetitions';
    is $m[0].map(*.Str).join(','), 'a,b,c', 'range **1..3 %: folded captures are correct';
}

# --- the IPv4 canary (nested quantified captures) ---
{
    my $m = "127.0.0.1" ~~ / (\d ** 1..3) ** 4 % '.' /;
    is $m[0].elems, 4, 'nested (\d**1..3)**4 % ".": four octets folded into group 0';
    is $m[0].map(*.Str).join('|'), '127|0|0|1', 'nested: octet captures are correct';
}

# --- `.match` exposes the same folded structure ---
{
    my $m = "10.20.30.40".match(/ (\d ** 1..3) ** 4 % '.' /);
    is $m[0].elems, 4, '.match: four octets folded into group 0';
    is $m[0][2].Str, '30', '.match: individual octet is addressable';
}

# --- plain `+ %` (already worked) still folds ---
{
    my $m = "a,b,c" ~~ / (\w)+ % ',' /;
    is $m[0].elems, 3, '+ %: group 0 folds all repetitions';
}
