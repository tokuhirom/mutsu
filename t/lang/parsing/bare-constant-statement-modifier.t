use Test;

plan 5;

# A bare (no `my`/`our`) `constant` declaration is dispatched directly from
# the top-level statement-parser table, which never applied a trailing
# `if`/`unless` statement modifier to it (unlike `my constant`/`our
# constant`, handled by a separate wrapper that does). Without the modifier
# being consumed, `constant $w = 12 if False;` left "if False;" completely
# unparsed, and the statement-list driver then tried to parse it as a brand
# new `if` control statement with no block, dying with
# `X::Syntax::Missing: Missing block`. See
# todo/tickets/bare-constant-if-modifier-missing-block.md.
#
# `constant` is resolved at compile time and (per real raku) evaluates
# unconditionally regardless of the modifier's condition -- matching the
# `my constant` sibling fix in
# news/2026-08/constant-statement-modifier-value-lost.md.

{
    constant $w1 = 12 if False;
    is $w1, 12, 'bare constant with a False if-modifier parses and keeps its value';
}

{
    constant $w2 = 12 if True;
    is $w2, 12, 'bare constant with a True if-modifier keeps its value';
}

{
    constant $w3 = 13 unless True;
    is $w3, 13, 'bare constant with a falsifying unless-modifier keeps its value';
}

{
    constant @a = (1, 2, 3) if False;
    is-deeply @a, (1, 2, 3), 'bare array constant with a False if-modifier keeps its value';
}

{
    constant $w5 = 99;
    is $w5, 99, 'a bare constant with no modifier is unaffected (regression guard)';
}
