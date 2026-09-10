use Test;

plan 3;

# `try` implicitly turns on `use fatal` for its own lexical scope
# (try-block-implicit-use-fatal.t), so a Failure produced *directly* inside a
# try body's tail expression is sunk-fatal there. But when the try's tail
# expression is instead the return value of a *separately declared* closure
# whose own body did NOT run under fatal, that returned value must stay soft
# — `use fatal` describes the state active when a Failure was actually
# created, not the ambient state of whoever later happens to receive it.
#
# This matters specifically for a `Seq`/`Slip` return value: `.map`'s own
# native loop already enforces `use fatal` at the correct, per-element time
# (`resolution_map_grep.rs`), so a later, ambient recheck of the returned
# list at the try's own sink point can only be *wrong* — it retroactively
# stamps the try's fatal-ness onto a list the callee legitimately built as
# soft.

# A closure with no `use fatal` of its own, returning a Seq whose lone
# element is an unhandled Failure: calling it as a try's tail statement must
# not explode.
{
    my &c = { "a".map: *.Int };
    my $died = False;
    try {
        c();
        CATCH { default { $died = True } }
    }
    ok !$died, 'try does not retroactively fatal a Seq a plain closure returned soft';
}

# Sanity check in the other direction: a closure that DOES declare its own
# `use fatal` still throws when called the same way (this is not a
# "fatal from map never fires" regression).
{
    my &f = { use fatal; "a".map: *.Int };
    my $died = False;
    try {
        f();
        CATCH { default { $died = True } }
    }
    ok $died, 'a closure that itself declares use fatal still throws its own Seq';
}

# A bare Failure (not list-wrapped) created directly in the try's own body
# still throws — this is try's own implicit-fatal doing its job, unrelated
# to the closure-boundary case above.
{
    my $died = False;
    try {
        "a".Int;
        CATCH { default { $died = True } }
    }
    ok $died, 'a bare Failure created directly in the try body still throws';
}

# NOTE: a *bare* (not list-wrapped) soft Failure created *before* a `try`,
# then merely read as the try's tail expression, is ALSO retroactively
# exploded by mutsu today (raku keeps it soft — creation-time semantics, not
# consumption-time). That is a separate, pre-existing gap in the unconditional
# `failure_to_runtime_error_if_unhandled` check (not the `use fatal`-gated
# list-descend check fixed here) and is out of scope for this file; see
# `todo/tickets/bare-failure-sink-is-consumption-time-not-creation-time.md`.
