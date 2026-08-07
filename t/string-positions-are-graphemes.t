use Test;

# A Raku string is a sequence of graphemes, so every position and length a
# string method reports or accepts is a grapheme index. `.chars` already counted
# graphemes; `substr`, `index`, `rindex` and `indices` counted codepoints, so
# the two scales disagreed on any string holding a multi-codepoint grapheme.
# `\r\n` is one such grapheme and is everywhere in wire protocols -- Cro's
# multipart/form-data parser walks a body with exactly `index` + `substr` and
# lost a character per boundary.

plan 18;

# --- CRLF is one grapheme.
{
    my $s = "AAA\r\n--bnd\r\nBBB";
    is $s.chars, 13, '.chars counts CRLF as one';
    is $s.index('--bnd'), 4, 'index reports a grapheme offset past a CRLF';
    is $s.index("\r\n"), 3, 'index finds a CRLF needle';
    is $s.index("\r\n", 4), 9, 'index honours a grapheme start offset';
    is $s.rindex("\r\n"), 9, 'rindex reports a grapheme offset';
    is $s.rindex('B'), 12, 'rindex past two CRLFs';
    is-deeply $s.indices("\r\n"), (3, 9), 'indices are grapheme offsets';
    is $s.substr(4, 5), '--bnd', 'substr slices by grapheme';
    is $s.substr(3), "\r\n--bnd\r\nBBB", 'substr with no length starts on a grapheme';
    is $s.substr(4), "--bnd\r\nBBB", 'substr past a CRLF does not tear it';
}

# A start offset that lands just after a CRLF must not split it.
{
    my $t = "\r\nHello";
    is $t.chars, 6, 'leading CRLF counts once';
    is $t.substr(1), 'Hello', 'substr(1) steps over the whole CRLF';
}

# --- A combining mark is one grapheme too.
{
    my $t = "a\c[LATIN SMALL LETTER E]\c[COMBINING ACUTE ACCENT]bc";
    is $t.chars, 4, 'a base plus a combining mark is one grapheme';
    is $t.index('b'), 2, 'index counts the combined grapheme once';
    is $t.rindex('c'), 3, 'rindex counts the combined grapheme once';
    is $t.substr(1, 1), "\c[LATIN SMALL LETTER E]\c[COMBINING ACUTE ACCENT]",
        'substr yields the whole grapheme';
}

# --- Plain ASCII is unchanged (the fast path).
{
    my $u = 'hello world';
    is $u.index('o'), 4, 'ASCII index is unchanged';
    is $u.substr(6, 5), 'world', 'ASCII substr is unchanged';
}
