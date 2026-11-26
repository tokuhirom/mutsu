use Test;

# `.comb`, `.lines`, `.words`, `.substr` and an ASCII subject's Match text
# share the invocant's buffer (`StrBody::Slice`) instead of copying it, as
# `.split` does since #19201. Sharing must stay invisible: every piece is an
# ordinary, independent Str, including pieces of a strand-list invocant
# (`x`, `~`).

plan 18;

my $s = "héllo wörld\nline two\r\nñ 😀x\n";
is-deeply $s.comb(3).List, ("hél", "lo ", "wör", "ld\n", "lin", "e t", "wo\r\n", "ñ 😀", "x\n"), 'comb(n)';
is-deeply $s.lines.List, ("héllo wörld", "line two", "ñ 😀x"), 'lines';
is-deeply $s.lines(:!chomp).List, ("héllo wörld\n", "line two\r\n", "ñ 😀x\n"), 'lines(:!chomp)';
is-deeply $s.words.List, ("héllo", "wörld", "line", "two", "ñ", "😀x"), 'words';
is-deeply $s.comb("l").List, ("l", "l", "l", "l"), 'comb(Str)';

is "abcdef".substr(1, 3), "bcd", 'substr(start, len)';
is substr("héllo", 1, 3), "éll", 'substr function, non-ASCII';
is "abcdef".substr(2, *-1), "cde", 'substr with a WhateverCode length';
is "abcdef".substr(1..3), "bcd", 'substr with a Range';

my $t = "ab cd\nef" x 3;
my @w = $t.words;
is-deeply @w, ["ab", "cd", "efab", "cd", "efab", "cd", "ef"], 'words of a strand-list invocant';
my $piece = @w[1];
$piece ~= "!";
is $piece, "cd!", 'appending to a piece';
is @w[1], "cd", 'leaves the other holder alone';
my $u = $t;
$u ~= "Z";
is @w[6], "ef", 'appending to the invocant leaves its pieces alone';
is $t.substr(0, 2), "ab", 'substr of a strand-list invocant';

my $q = "q" x 3;
my $p = $q.substr(1);
$q ~= "w";
is "$q $p", "qqqw qq", 'a substr outlives an append to its invocant';

my $m = "foo bar baz" ~~ /(b\w+) \s (b\w+)/;
is "$m|$m[0]|$m[1]", "bar baz|bar|baz", 'Match and capture text on an ASCII subject';
is "aaa".subst(/(a)/, -> $/ { $0.uc }, :g), "AAA", 'captures in a substitution';

my $big = "word " x 20000;
is $big.substr(99995), "word ", 'a small substr of a large invocant';
