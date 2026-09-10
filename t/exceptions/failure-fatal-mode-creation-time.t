use Test;

# Raku decides whether an unhandled Failure throws under `use fatal` at the
# moment the Failure is CONSTRUCTED, using whichever fatal state is active
# then -- not at the moment it is later read/sunk. A Failure made outside
# `use fatal` stays soft forever, even when a bare mention of it is later
# reached from inside a scope where fatal is now on (a bare variable mention
# never forces sink in Raku -- "Useless use of ... in sink context" -- so it
# cannot retroactively explode a Failure that was legitimately soft at birth).
# See todo/tickets/bare-failure-sink-is-consumption-time-not-creation-time.md.

plan 6;

# 1. Soft Failure created outside fatal, sunk (bare var) inside a later
#    `use fatal` scope: must NOT throw (the ticket's minimal repro).
sub created_outside_sunk_inside_fatal() {
    my $f = "a".Int;
    {
        use fatal;
        $f;
    }
    return "reached";
}
is created_outside_sunk_inside_fatal(), "reached",
    "Failure created without fatal stays soft when sunk in a later use fatal scope";

# 2. Same shape with no `use fatal` anywhere at all in the program -- a bare
#    variable mention never forces sink regardless of fatal state.
sub created_and_sunk_without_fatal() {
    my $f = "a".Int;
    $f;
    return "reached";
}
is created_and_sunk_without_fatal(), "reached",
    "a bare variable holding an unhandled Failure never forces sink";

# 3. Inverse: a Failure created INSIDE a `use fatal` scope explodes
#    immediately at construction time, so it never survives to become a
#    soft, later-sinkable value at all.
sub created_inside_fatal() {
    use fatal;
    my $f = "a".Int;
    return "unreached";
}
dies-ok { created_inside_fatal() },
    "Failure created under use fatal throws immediately at construction";

# 4. Existing behavior is unchanged: created and sunk in the SAME fatal
#    scope still explodes.
sub created_and_sunk_same_fatal_scope() {
    use fatal;
    my $f = "a".Int;
    $f;
    return "unreached";
}
dies-ok { created_and_sunk_same_fatal_scope() },
    "Failure created and sunk in the same use fatal scope still throws";

# 5. A *fresh* (non-bare-variable) sink of an unhandled Failure still throws
#    regardless of fatal -- only a bare-variable mention is exempt.
dies-ok { "a".Int; },
    "a fresh coercion Failure sunk directly still throws without use fatal";

# 6. The explicit `sink` statement-prefix forces even a bare variable.
sub explicit_sink_forces() {
    my $f = "a".Int;
    sink $f;
    return "unreached";
}
dies-ok { explicit_sink_forces() },
    "explicit 'sink \$f' forces a bare variable's Failure to explode";
