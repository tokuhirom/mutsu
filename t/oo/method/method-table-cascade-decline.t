use Test;

# ADR-11276 §9.88: the name-matching cascades decline a call the built-in
# method table answers, and the one-argument entry asks the table first, so
# the arms that only forwarded to a row's handler are gone. Every route a
# call can take must still reach the row: a method call, a routine form that
# calls the method, `.^can`, and a `.wrap` of the built-in method.

plan 15;

subtest 'Capture rows and the arms that stay' => {
    my $c = \(1, 2, :a(3));
    is-deeply $c.keys.List, (0, 1, 'a'), 'keys';
    is-deeply $c.values.List, (1, 2, 3), 'values';
    is-deeply $c.hash, Map.new((a => 3)), 'hash';
    is-deeply $c.list, (1, 2), 'list';
    is $c.elems, 2, 'elems';
    is $c.Numeric, 2, 'Numeric';
    is $c.gist, '\(1, 2, :a(3))', 'gist';
    is $c.raku, '\(1, 2, :a(3))', 'raku';
    is-deeply $c.Hash, {a => 3}, 'Hash (no row: the cascade arm)';
    ok $c.Bool, 'Bool';
    nok \().Bool, 'Bool of an empty Capture';
}

subtest 'Uni rows' => {
    my $u = "éa".NFD;
    is $u.elems, 3, 'elems';
    is $u.codes, 3, 'codes';
    is $u.Str, "éa", 'Str';
    is-deeply $u.list, (101, 769, 97), 'list';
}

subtest 'Version rows' => {
    is-deeply (v1.2.3).parts, (1, 2, 3), 'parts';
    nok (v1.2).plus, 'plus';
    ok (v1.2+).plus, 'plus of v1.2+';
    ok (v1.*.3).whatever, 'whatever';
}

subtest 'Complex and Pair rows' => {
    is (1+2i).re, 1e0, 're';
    is (1+2i).im, 2e0, 'im';
    is-deeply (1+2i).reals, (1e0, 2e0), 'reals';
    is (a => 1).key, 'a', 'key';
    is (a => 1).value, 1, 'value';
    is-deeply (a => 1).antipair, (1 => 'a'), 'antipair';
    is True.key, 'True', 'Bool.key';
}

subtest 'a Pair yielded by a hash loop reads the live value' => {
    my %h = a => 1;
    my @seen;
    for %h -> $p { %h{$p.key} = 10; @seen.push: $p.value }
    is-deeply @seen, [10], 'value follows the write';
}

subtest 'squish and contents' => {
    is-deeply [1, 1, 2, 2, 1].squish.List, (1, 2, 1), 'Array.squish';
    is-deeply (1, 1, 2).squish.List, (1, 2), 'List.squish';
    is-deeply buf8.new(1, 2).contents, (1, 2), 'Buf.contents';
    is-deeply Blob.new(7).contents, (7,), 'Blob.contents';
}

subtest 'text search rows with one argument' => {
    is "abcabc".index("c"), 2, 'index';
    is "abcabc".rindex("c"), 5, 'rindex';
    is-deeply "abc".index("z"), Nil, 'index of a miss';
    ok "abc".contains("b"), 'contains';
    ok "abc".contains("a" | "z"), 'contains with a Junction needle';
    ok "abc".contains(/b/), 'contains with a Regex needle';
    is 42.index(2), 1, 'Cool.index';
    is 1234.substr(1), '234', 'Cool.substr';
    is 1234.substr(*-2), '34', 'substr with a WhateverCode';
}

subtest 'split, comb, fmt, sprintf with one argument' => {
    is-deeply "a,b,c".split(",").List, <a b c>, 'split on a Str';
    is-deeply "a1b2".split(/\d/).List, ('a', 'b', ''), 'split on a Regex';
    is-deeply 123.split(2).List, ('1', '3'), 'Cool.split';
    is-deeply "abcd".comb(2).List, <ab cd>, 'comb by count';
    is-deeply "aXbX".comb("X").List, <X X>, 'comb by Str';
    is (1, 2).fmt("%02d"), '01 02', 'List.fmt';
    is 3.fmt("%03d"), '003', 'Int.fmt';
    is "%x".sprintf(255), 'ff', 'sprintf';
    is "%s-%s".sprintf([1, 2]), '1-2', 'sprintf spreads a list';
}

subtest 'base, base-repeating and subbuf with one argument' => {
    is 255.base(16), 'FF', 'Int.base';
    is 3.5.base(2), '11.1', 'Rat.base';
    is Duration.new(10).base(2), '1010', 'Duration.base';
    is-deeply (1/3).base-repeating(10), ('0.', '3'), 'base-repeating';
    is-deeply Buf.new(1, 2, 3).subbuf(1), Buf.new(2, 3), 'subbuf from an offset';
    is-deeply Blob.new(1, 2, 3).subbuf(*-1), Blob.new(3), 'subbuf with a WhateverCode';
}

subtest 'a routine form reaches the row' => {
    is-deeply combinations(3, 2).map(*.List).List, ((0, 1), (0, 2), (1, 2)),
        'combinations($n, $k)';
    is index("abc", "c"), 2, 'index($s, $needle)';
    is substr("abc", 1), 'bc', 'substr($s, $from)';
}

subtest '.^can sees the rows' => {
    ok "abc".^can('index'), 'Str can index';
    ok (1+2i).^can('re'), 'Complex can re';
    ok \(1).^can('keys'), 'Capture can keys';
}

subtest 'a wrap of a row-owned method wins' => {
    my $h1 = Capture.^find_method('keys').wrap(-> | { "cap-keys" });
    my $h2 = Str.^find_method('index').wrap(-> | { "str-index" });
    my $h3 = Complex.^find_method('re').wrap(-> | { "cpx-re" });
    is \(1, :a(2)).keys, 'cap-keys', 'Capture.keys';
    is "abc".index("b"), 'str-index', 'Str.index';
    is (1+2i).re, 'cpx-re', 'Complex.re';
    $h1.restore; $h2.restore; $h3.restore;
    is-deeply \(1).keys.List, (0,), 'restored';
}

subtest 'a Match answers its rows' => {
    grammar G { token TOP { <a> <b> }; token a { \d+ }; token b { \w+ } }
    my $m = G.parse("12ab");
    is $m.from, 0, 'from';
    is $m.to, 4, 'to';
    is $m.Str, '12ab', 'Str';
    is $m<a>.prematch, '', 'prematch';
    is $m<a>.postmatch, 'ab', 'postmatch';
    is $m.gist.lines.elems, 3, 'gist';
    is-deeply $m.clone.Str, '12ab', 'clone';
    my $plain = "ab12cd" ~~ /(\d)(\d)/;
    is $plain.caps.elems, 2, 'caps';
    is-deeply $plain.chunks.map(*.key).List, (0, 1), 'chunks';
    is $plain.made, Nil, 'made';
}

subtest 'a lazy match answers the scalar accessors' => {
    my $c;
    "xy" ~~ / x { $c = $¢ } y /;
    is $c.from, 0, 'from of a cursor';
    is $c.orig, 'xy', 'orig of a cursor';
}

subtest 'Str.Str with the script adverbs' => {
    is 42.Str(:superscript), '⁴²', ':superscript';
    is 42.Str(:subscript), '₄₂', ':subscript';
}
