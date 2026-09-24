use Test;

# `Str.comb` / `.lines` / `.words` return a Seq over a string cursor
# (`SeqSource::StrIter`): `.head(n)`, `.first` and a bounded subscript cut
# only the prefix they need. These pin that the prefix paths give the same
# answers, and keep the same Seq consumption rules, as reading the whole Seq.

plan 34;

my $s = "héllo wörld\nfoo  bar\r\nbaz\n";

# --- prefix consumers agree with the full list ----------------------------
is-deeply $s.comb.head(3), $s.comb.List.head(3), '.comb.head(3)';
is-deeply $s.comb(3).head(2), ("hél", "lo ").Seq, '.comb(Int).head';
is-deeply $s.comb("o").head(5), ("o", "o", "o").Seq, '.comb(Str).head past the end';
is-deeply $s.lines.head(2), ("héllo wörld", "foo  bar").Seq, '.lines.head';
is-deeply $s.lines(:!chomp).head(2), ("héllo wörld\n", "foo  bar\r\n").Seq, '.lines(:!chomp).head';
is-deeply $s.words.head(4), ("héllo", "wörld", "foo", "bar").Seq, '.words.head';
is-deeply $s.words(2).head(5), ("héllo", "wörld").Seq, '.words($limit).head';
is-deeply lines($s).head(1), ("héllo wörld",).Seq, 'sub lines(...).head';
is-deeply words($s).head(1), ("héllo",).Seq, 'sub words(...).head';
is $s.comb.first, "h", '.comb.first';
is $s.words.first, "héllo", '.words.first';
is $s.comb.head, "h", '.comb.head with no count';
is-deeply "".comb.head(2), ().Seq, 'empty string';
is-deeply "abc".comb.head(0), ().Seq, 'head(0)';
is-deeply "abc".comb.head(*), ("a", "b", "c").Seq, 'head(*) takes everything';
is-deeply "abc".comb.head(-1), ().Seq, 'head(-1) takes nothing';
is-deeply "\x[1F1EF]\x[1F1F5]e\x[301]".comb.head(2), ("\x[1F1EF]\x[1F1F5]", "e\x[301]").Seq,
    'graphemes stay whole';

# --- subscripts ------------------------------------------------------------
is "abcd".comb[1], "b", 'temporary subscript';
is-deeply "abcd".comb[^2], ("a", "b"), 'temporary range subscript';
is "a\nb\nc".lines[2], "c", 'lines subscript';
my $v = "abcd".comb;
is $v[3], "d", 'subscript on a variable';
is $v[9], Nil, 'subscript past the end';
is-deeply $v.List, ("a", "b", "c", "d"), 'the whole Seq is still there after a subscript';

# --- consumption rules match an eagerly built Seq -------------------------
my $h = "abc".comb;
$h.head(2);
throws-like { $h.List }, X::Seq::Consumed, '.head consumes the Seq';
my $f = "abc".comb;
$f.first;
throws-like { $f.List }, X::Seq::Consumed, '.first consumes the Seq';
my $c = "abc".comb.cache;
is-deeply $c.head(1), ("a",).Seq, '.head on a cached Seq';
is-deeply $c.List, ("a", "b", "c"), 'a cached Seq is not consumed by .head';
my $e = "abc".comb;
is $e.elems, 3, '.elems';
is-deeply $e.List, ("a", "b", "c"), '.elems does not consume';

# --- whole-Seq readers --------------------------------------------------
my $w = "a b c".words;
is $w.raku, '$(("a", "b", "c").Seq)', 'an itemized Seq keeps its $( ) in .raku';
nok "".comb, 'an empty comb is False';
ok "x".comb, 'a non-empty comb is True';
is "a\nb\nc".lines(:count), 3, '.lines(:count)';
is ~"abc".comb, "a b c", 'Str of the Seq';
