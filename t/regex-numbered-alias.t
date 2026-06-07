use Test;

# Numbered scalar capture aliases (`$N=<atom>`) and `:s` (sigspace) propagation
# into alternation branches. See roast/S05-capture/alias.t.

plan 16;

# --- Reverse / explicit numbered aliases ------------------------------------

ok "abcd" ~~ m/a $1=(.) $0=(.) d/, 'reverse numbered alias matches';
is ~$0, 'c', '$0 captured (second group)';
is ~$1, 'b', '$1 captured (first group)';

# Numbered alias starting at an arbitrary index, with auto-numbering continuing.
ok "foobar" ~~ m/$42=. (..) (...) /, 'alias starting at non-zero matches';
is ~$42, 'f',   '$42 explicit';
is ~$43, 'oo',  '$43 continues numbering';
is ~$44, 'bar', '$44 continues numbering';

# A bare `$0` (no `=`) is still a backreference, not an alias.
ok "abab" ~~ m/(ab) $0/, 'bare $0 is a backreference';

# --- :s (sigspace) propagates into alternation branches ---------------------

ok "a b" ~~ m:s/(a) (b) | (c) (d)/, ':s matches first multi-token alternative';
ok "c d" ~~ m:s/(a) (b) | (c) (d)/, ':s matches second multi-token alternative';
nok "ab" ~~ m:s/(a) (b) | (c) (d)/, ':s requires whitespace between tokens';

# Numbered aliases inside :s alternation (the alias.t pair-match shape).
ok "foo => 22" ~~ m:s/$0=(foo) '=>' (\d+) | $1=(\d+) '<=' $0=(foo)/,
    ':s + alias, first alternative';
is ~$0, 'foo', 'key captured via $0=';
is ~$1, '22',  'value captured via (\d+)';

ok "22 <= foo" ~~ m:s/$0=(foo) '=>' (\d+) | $1=(\d+) '<=' $0=(foo)/,
    ':s + alias, second alternative';
is ~$0, 'foo', 'reverse key captured via $0=';
