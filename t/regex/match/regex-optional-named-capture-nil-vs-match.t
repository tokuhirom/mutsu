use Test;

# An unmatched named capture's Nil-ness depends on WHERE the `?` sits, not
# just on whether the name matched zero times (verified against `raku`):
#
# - `$<x>=[...]?` / `$<x>=<[...]>?` -- the `?` quantifies the SAME token the
#   name is attached to, so the token "runs" as a unit even on its zero
#   branch: `$<x>` is a defined, empty (zero-width) Match.
# - `(x)?` / `$<x>=(...)?` -- the `?` quantifies a CaptureGroup atom, and
#   Raku still yields Nil for a capturing group that matched zero times, even
#   when the group itself carries the name.
# - `$<x>=<.subrule>?` / `$<x>=<subrule>?` -- a BARE subrule call (including a
#   zero-arg builtin like `<lower>`) is a third case, distinct from the
#   character-class one above even though both are written `$<x>=<...>?`: an
#   unmatched subrule never produced a Match to bind, so `$<x>` is Nil and
#   absent from `.caps`, exactly like the CaptureGroup case. Wrapping the same
#   subrule call in a group (`$<x>=[<.subrule>]?`) reverts to the
#   non-capturing-group case above and yields a defined, empty Match --
#   verified against `raku` (issue #9054).
#
# Each scenario uses its own capture name (rather than reusing `$<x>` across
# blocks) because a named capture absent from the current match falls back to
# a stale value from an earlier match in this file -- a real, separate bug
# (todo/tickets/named-capture-absent-from-current-match-leaks-stale-value.md)
# that this test intentionally does not exercise.

plan 11;

if "b" ~~ / $<ncg>=[<[cd]>]? "b" / {
    ok $<ncg>.defined, 'name on a non-capturing group: defined (empty Match)';
    is ~$<ncg>, '', 'name on a non-capturing group: stringifies to empty string';
} else {
    flunk 'name on a non-capturing group: match';
    flunk 'name on a non-capturing group: stringify';
}

if "b" ~~ / $<bare>=<[cd]>? "b" / {
    ok $<bare>.defined, 'name on a bare quantified atom: defined (empty Match)';
    is ~$<bare>, '', 'name on a bare quantified atom: stringifies to empty string';
} else {
    flunk 'name on a bare atom: match';
    flunk 'name on a bare atom: stringify';
}

if "b" ~~ / $<cg>=(<[cd]>)? "b" / {
    nok $<cg>.defined, 'name on a capturing group: Nil (group did not run)';
} else {
    flunk 'name on a capturing group: match';
}

if "b" ~~ /(<[ab]> $<outer>=<[cd]>)? "b"/ {
    nok $<outer>.defined, 'name inside an unmatched outer group: Nil';
} else {
    flunk 'name inside an unmatched outer group: match';
}

# A matched (non-zero) named capture is unaffected by any of the above.
if "bc" ~~ / "b" $<ncg2>=[<[cd]>]? / {
    is ~$<ncg2>, 'c', 'a matched non-capturing-group name still captures its text';
} else {
    flunk 'matched non-capturing group name';
}

if "cb" ~~ / $<bare2>=<[cd]>? "b" / {
    is ~$<bare2>, 'c', 'a matched bare-atom name still captures its text';
} else {
    flunk 'matched bare atom name';
}

grammar SubruleG {
    token digit { \d }
    token TOP {
        $<sub>=<.digit>? 'x'
        $<subgroup>=[<.digit>]? 'y'
    }
}

if SubruleG.parse('xy') -> $m {
    nok $m<sub>.defined, 'name on a bare unmatched subrule call: Nil';
    ok $m<subgroup>.defined,
        'name on a group wrapping an unmatched subrule call: defined (empty Match)';
} else {
    flunk 'bare subrule call: parse';
    flunk 'grouped subrule call: parse';
}

if SubruleG.parse('5x6y') -> $m {
    is ~$m<sub>, '5', 'a matched bare subrule call name still captures its text';
} else {
    flunk 'matched bare subrule call';
}
