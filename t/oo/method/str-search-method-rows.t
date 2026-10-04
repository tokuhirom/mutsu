use Test;

# ADR-11276 slice 3: `contains`, `starts-with`, `ends-with`, `index`,
# `rindex` (one needle) and `substr` (a start and an optional length) are
# rows owned by `Str` and `Cool`, the first rows that take arguments. A call
# on a variable with plain scalar arguments is answered by the call-site
# lane; any other argument (a Junction, a Regex, a named argument, a
# WhateverCode or Range position) takes the full path.

plan 9;

subtest 'plain Str, repeated through one site', {
    plan 3;
    my $s = "hello world";
    my $n = "o";
    my @got;
    for ^3 {
        @got.append: ($s.contains($n), $s.starts-with("he"), $s.ends-with("ld"),
                    $s.index($n), $s.rindex($n), $s.substr(6), $s.substr(1, 3));
    }
    my @want = True, True, True, 4, 7, "world", "ell";
    is-deeply @got[^7].List, @want.List, 'first iteration';
    is-deeply @got[7..^14].List, @want.List, 'second iteration';
    is-deeply @got[14..^21].List, @want.List, 'third iteration';
}

subtest 'misses', {
    plan 5;
    my $s = "hello";
    is-deeply $s.contains("z"), False, 'contains';
    is-deeply $s.index("z"), Nil, 'index';
    is-deeply $s.rindex("z"), Nil, 'rindex';
    is-deeply $s.starts-with("lo"), False, 'starts-with';
    is-deeply $s.ends-with("he"), False, 'ends-with';
}

subtest 'numeric needles stringify', {
    plan 3;
    my $s = "a1.5b";
    is-deeply $s.contains(1.5), True, 'a Rat needle';
    is-deeply $s.index(5), 3, 'an Int needle';
    is-deeply "x2y".contains(2e0), True, 'a Num needle reads as "2"';
}

subtest 'Cool receivers', {
    plan 4;
    my $i = 12345;
    is-deeply $i.substr(1, 2), "23", 'Int.substr';
    is-deeply $i.contains(34), True, 'Int.contains';
    is-deeply (^3).map({ $i.index(4) }).List, (3, 3, 3), 'Int.index through the lane';
    is-deeply 1.5.starts-with("1."), True, 'Rat.starts-with';
}

subtest 'graphemes', {
    plan 3;
    my $s = "aéb\x[1F600]c";
    is-deeply $s.index("b"), 2, 'index counts graphemes';
    is-deeply $s.substr(3, 1), "\x[1F600]", 'substr slices graphemes';
    is-deeply $s.rindex("c"), 4, 'rindex counts graphemes';
}

subtest 'arguments the rows do not take still work', {
    plan 6;
    my $s = "hello world";
    ok so($s.contains("o" | "z")) && !so($s.contains("q" | "z")), 'a Junction needle autothreads';
    is-deeply $s.contains(/w./), True, 'a Regex needle';
    is-deeply $s.contains("WORLD", :i), True, 'a named argument';
    is-deeply $s.substr(*-3), "rld", 'a WhateverCode start';
    is-deeply $s.substr(1..2), "el", 'a Range';
    is-deeply $s.index("o", 5), 7, 'a two-argument index';
}

ok "hello".substr(10) ~~ Failure, 'a start past the end answers a Failure';

{
    sub find($s, $n) { $s.index($n) }
    is-deeply (find("abc", "c"), find("xyz", "x"), find("abc", "q")), (2, 0, Nil),
        'one site, changing receivers and needles';
}

ok Str.^can('substr') && Cool.^can('contains'), 'introspection sees the methods';
