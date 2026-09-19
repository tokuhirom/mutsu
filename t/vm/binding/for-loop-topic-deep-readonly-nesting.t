use v6;
use Test;

# ADR-0097 slice 4: the implicit `for` topic's "deep readonly" mark used to be
# a SECOND, independent fact -- a raw `__mutsu_deep_readonly::_` env string
# written and removed at six separate call sites in `vm_for_loop_body.rs`,
# alongside (but not part of) the `ReadonlyKind::Immutable` mark that
# `readonly_vars` already tracks with proper scope-exit undo. Two side effects
# of keeping it separate:
#
#   * every one of those six sites had to remember to touch BOTH the
#     `readonly_vars` mark and the env key, and
#   * `restore_topic_readonly` restored only the `ReadonlyKind` half and
#     *always* cleared the env key regardless of what the surrounding scope
#     needed -- so a `for`-loop over an immutable QuantHash whose body ran a
#     NESTED `for`-loop over its own implicit topic lost its own deep-readonly
#     mark the moment the inner loop exited, even though the outer loop's own
#     topic was never reassigned.
#
# `ReadonlyKind::ImmutableDeep` folds the two into one mark, so there is
# nothing left to forget and nothing left to restore separately.
plan 6;

# A mutable BagHash's `.pairs` topic is NOT deep-readonly: `.value = ...`
# writes back into the BagHash (the shallow/deep split this pins).
{
    my $bh = (a => 1, b => 1).BagHash;
    for $bh.pairs {
        .value = 5;
    }
    is $bh<a>, 5, 'a mutable BagHash pairs topic writes .value= back (a)';
    is $bh<b>, 5, 'a mutable BagHash pairs topic writes .value= back (b)';
}

# An immutable Bag's `.pairs` topic IS deep-readonly: `.value = ...` dies.
{
    my $b = (a => 1, b => 2).Bag;
    my $died = 0;
    for $b.pairs {
        $died++ if !try { .value = 5; True };
    }
    is $died, 2, 'an immutable Bag pairs topic refuses .value= on every item';
}

# The regression this file exists for: an outer loop over an immutable Mix's
# `.pairs`, with an INNER loop (over a plain, non-QuantHash list) nested in the
# body. The inner loop also binds the implicit topic and restores it on exit --
# that restore must not erase the OUTER loop's still-live deep-readonly mark.
{
    my $m = (a => 1, b => 2).Mix;
    my $died = 0;
    for $m.pairs {
        for 1, 2 { }  # unrelated inner loop; rebinds and restores $_
        $died++ if !try { .value = 99; True };
    }
    is $died, 2,
        'an outer immutable-Mix topic stays deep-readonly across a nested plain-list loop';
}

# Same shape, but the inner loop is itself over another immutable QuantHash,
# so BOTH loops mark the topic ImmutableDeep -- the inner exit must restore
# the outer's deep mark, not merely "some" readonly mark.
{
    my $outer = (a => 1).Mix;
    my $inner = (x => 1).Set;
    my $died = 0;
    for $outer.pairs {
        for $inner.pairs { }
        $died++ if !try { .value = 1; True };
    }
    is $died, 1,
        'an outer immutable-Mix topic stays deep-readonly across a nested immutable-Set loop';
}

# A plain (non-deep) readonly topic -- iterating a literal list -- keeps its
# usual "Cannot assign to an immutable value" `$_ = ...` refusal, unaffected
# by folding the deep flag into the same mark.
{
    my $died = 0;
    for 1, 2 {
        $died++ if !try { $_ = 9; True };
    }
    is $died, 2, 'a plain literal-list topic still refuses $_ = ... (unrelated to deep)';
}
