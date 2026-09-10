use Test;

# `.comb` Int-chunk and Str-fixed matchers are a single pure implementation in
# builtins::comb, shared by the native fast path and the interpreter. The Regex
# matcher stays in the interpreter (regex engine). These exercise both layers and
# the same construct under EVAL (interpreter carrier).

plan 18;

# --- Int chunk matcher (native) ---
is "aabbcc".comb(2).join('|'), 'aa|bb|cc', 'comb(Int) chunks';
is "abcde".comb(2).join('|'), 'ab|cd|e', 'comb(Int) trailing short chunk';
is "aabbcc".comb(2, 2).join('|'), 'aa|bb', 'comb(Int, limit)';
is "aaa".comb(2, 0).elems, 0, 'comb(Int, 0) is empty';
is "abc".comb(-1).join('|'), 'a|b|c', 'comb(Int<=0) is per-grapheme';

# --- Str fixed matcher (native) ---
is "abcabc".comb("bc").join('|'), 'bc|bc', 'comb(Str) fixed needle';
is "a.b.c".comb(".").join('|'), '.|.', 'comb(Str) literal dot';
is "hello".comb("").join('|'), 'h|e|l|l|o', 'comb("") splits graphemes';
is "xaxax".comb("a", 1).join('|'), 'a', 'comb(Str, limit)';
is "abc".comb("z").elems, 0, 'comb(Str) no match is empty';

# --- no-arg grapheme split (native 0-arg) ---
is "héllo".comb.elems, 5, 'comb() grapheme count';

# --- Regex matcher (interpreter path) ---
is "a1b2c3".comb(/\d/).join('|'), '1|2|3', 'comb(/rx/) single digits';
is "foo123bar456".comb(/\d+/).join('|'), '123|456', 'comb(/rx/) digit runs';
is "a1b2c3".comb(/\d/, 2).join('|'), '1|2', 'comb(/rx/, limit)';

# --- Code matcher is rejected (interpreter path) ---
dies-ok { "abc".comb({ $_ }) }, 'comb(Code) dies';

# --- EVAL carrier (interpreter evaluates the call) ---
is EVAL(q{"aabbcc".comb(2).join('|')}), 'aa|bb|cc', 'comb(Int) via EVAL';
is EVAL(q{"a1b2".comb(/\d/).join('|')}), '1|2', 'comb(/rx/) via EVAL';

# --- Unicode grapheme chunking (native, combining marks) ---
is "a\c[COMBINING ACUTE ACCENT]bc".comb(1).elems, 3, 'comb(1) counts graphemes not codepoints';
