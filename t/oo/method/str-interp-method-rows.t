use Test;
plan 31;

# `match` on `Str`, `subst`/`trans`/`indices`/`IO` on `Str` and `Cool`, and
# `sprintf`/`printf` on `Cool` are interpreter rows (ADR-11276 §9.75).
# Expectations checked against raku.
is "hello".subst("l", "L"), "heLlo", 'Str.subst with a string';
is "hello".subst("l", "L", :g), "heLLo", 'Str.subst :g';
is "hello".subst(/l+/, "X"), "heXo", 'Str.subst with a regex';
is "hello".subst(/(l)/, { $0.uc }, :g), "heLLo", 'a closure replacement';
is "hello".subst(/l/), "helo", 'the replacement defaults to the empty string';
is 12321.subst(2, 9, :g), "19391", 'Cool.subst stringifies';
is (1, 2, 3).subst(" ", "-", :g), "1-2-3", 'List.subst';
is "aaa".subst("a", "b", :nth(2)), "aba", 'Str.subst :nth';
my $s = "abc";
$s.subst(/b/, "B");
is ~$/, "b", 'a regex subst sets $/';
"zz" ~~ /z/;
"abc".subst("b", "B");
is ~$/, "z", 'a string subst leaves $/ alone';

is "hello".trans("l" => "L"), "heLLo", 'Str.trans with a positional Pair';
is "hello".trans(<l o> => <L O>), "heLLO", 'lists of characters';
is "hello".trans("a..z" => "A..Z"), "HELLO", 'ranges';
is "hello".trans("l" => "", :d), "heo", ':d deletes';
is 123.trans("1" => "9"), "923", 'Cool.trans';
is "abc".trans, "abc", 'no mapping';

is "hello".indices("l"), (2, 3), 'Str.indices';
is "hello".indices("l", 3), (3,), 'Str.indices from a position';
is "aaa".indices("aa", :overlap), (0, 1), ':overlap';
is "AbA".indices("a", :i), (0, 2), ':i';
is 1221.indices(2), (1, 2), 'Cool.indices';

isa-ok "foo".IO, IO::Path, 'Str.IO';
is 42.IO.basename, "42", 'Cool.IO';
is "foo".IO(:CWD("/tmp")).Str, "foo", ':CWD is accepted and not forwarded';

is "%05d".sprintf(42), "00042", 'Cool.sprintf';
is "%s-%s".sprintf("a", "b"), "a-b", 'two arguments';
is "plain".sprintf, "plain", 'no argument';

is "abc".match(/b/).from, 1, 'Str.match';
is "abcb".match(/b/, :g).elems, 2, 'Str.match :g';
nok "abc".match(/x/), 'no match';
is "a-b".match(/(\w)/, :g).map(~*[0]).join, "ab", 'the captures';
