use Test;

# The implicit topic of a bare block is read-only exactly when the item it is
# bound to has no container of its own. Both halves are pinned here so a future
# change has to keep them honest at once: over-marking invents a throw rakudo
# does not have, under-marking silently drops a write rakudo rejects.
# Every expectation below was measured against rakudo 2026.06.
plan 31;

# --- must throw: the topic aliases an immutable item -------------------------

throws-like { (1, 2).map({ $_ = 5 }).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    '.map over a list literal binds an immutable topic';

throws-like { (1, 2).grep({ $_ = 5 }).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    '.grep over a list literal binds an immutable topic';

throws-like { (1 .. 3).map({ $_ = 5 }).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    '.map over a Range binds an immutable topic';

throws-like { my %h = a => 1; %h.keys.map({ $_ = "z" }).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    '.map over %h.keys binds an immutable topic';

throws-like { my $s = { $_ = 5 }; $s(7) }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'a bare block called with a literal binds an immutable topic';

throws-like { my $s = { $_ = 5 }; $s(3 + 4) }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'a bare block called with an arithmetic result binds an immutable topic';

throws-like { my &s = { $_ = 5 }; s(7) }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'a &-sigiled block called with a literal binds an immutable topic';

# A compound assignment to the same topic is rejected too. rakudo reports these
# as X::Assignment::RO ("Cannot modify an immutable Str (a)") rather than the
# X::AdHoc a plain `=` gets; mutsu's compound-assign paths still answer with
# their own wording, so only the rejection itself is pinned here (see the
# "Messages that are close but not exact" section of
# todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md).
dies-ok { ("a", "b").map({ $_ ~= "!" }).eager },
    'a compound assignment to a literal-list topic is rejected too';
dies-ok { ("a", "b").map({ $_ .= uc }).eager },
    'a .= mutation of a literal-list topic is rejected too';

throws-like { (map { $_ = 5 }, 1, 2).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'the listop map over literals binds an immutable topic';

throws-like { (grep { $_ = 5 }, 1, 2).eager }, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'the listop grep over literals binds an immutable topic';

# --- must stay writable: the topic aliases a real element --------------------

{
    my @a = 1, 2, 3;
    map { $_ = 5 }, @a;
    is-deeply @a, [5, 5, 5], 'the listop map over a named array still writes back';
}


{
    my @a = 1, 2, 3;
    @a.map({ $_ *= 10 }).eager;
    is-deeply @a, [10, 20, 30], '@a.map({ $_ *= 10 }) still writes back';
}

{
    my @a = 1, 2, 3;
    @a.map({ $_ = 5 }).eager;
    is-deeply @a, [5, 5, 5], '@a.map({ $_ = 5 }) still writes back';
}

{
    my @a = 1, 2, 3;
    @a.grep({ $_ = 5 });
    is-deeply @a, [5, 5, 5], '@a.grep({ $_ = 5 }) still writes back';
}

{
    my %h = a => 1;
    %h.values.map({ $_ = 9 }).eager;
    is %h<a>, 9, '%h.values.map({ $_ = 9 }) still writes back';
}

{
    my @a = 1, 2, 3;
    @a.values.map({ $_ = 7 }).eager;
    is-deeply @a, [7, 7, 7], '@a.values.map({ $_ = 7 }) still writes back';
}

{
    my $v = 1;
    my $b = { $_ = 9 };
    lives-ok { $b($v) }, 'a bare block called with a variable keeps a writable topic';
}

{
    my @a = 1, 2, 3;
    my $b = { $_ = 9 };
    lives-ok { $b(@a[0]) }, 'a bare block called with an element keeps a writable topic';
}

is (1, 2).map({ $_ * 2 }).eager, (2, 4), 'reading the topic over a literal is unaffected';
is (1, 2).grep({ $_ > 1 }).eager, (2,), 'reading the grep topic over a literal is unaffected';
is (1, 2).map(-> $v { $v + 1 }).eager, (2, 3),
    'a pointy block over a literal binds its own parameter, not the topic';
is (1, 2).map({ $^a + 1 }).eager, (2, 3),
    'a placeholder block over a literal keeps its own parameter';

# --- the mark must not leak into a nested topic binding ----------------------

sub topic-writer() { $_ = 5; "written" }

{
    my @b = 7, 8;
    (1, 2).map({ for @b { $_ = 1 } }).eager;
    is-deeply @b, [1, 1], 'an inner for over a real array stays writable inside a literal map';
}

is (1, 2).map({ topic-writer() }).eager, ("written", "written"),
    'a routine called from a literal-map block gets its own writable $_';

is (1, 2).grep({ topic-writer() }).elems, 2,
    'a routine called from a literal-grep block gets its own writable $_';

{
    my @a = 1, 2, 3;
    for 1, 2 { @a.map({ $_ = 5 }).eager }
    is-deeply @a, [5, 5, 5], 'an rw .map inside a for over a literal stays writable';
}

{
    my @a = 1, 2, 3;
    for 1, 2 { @a.grep({ $_ = 5 }) }
    is-deeply @a, [5, 5, 5], 'an rw .grep inside a for over a literal stays writable';
}

{
    my @b = 7, 8;
    for 1, 2 { for @b { $_ = 1 } }
    is-deeply @b, [1, 1], 'an inner for over a real array stays writable inside a literal for';
}

{
    lives-ok { for 1, 2 { topic-writer() } },
        'a routine called from a for over a literal gets its own writable $_';
}

# ...and the outer mark comes back afterwards.
throws-like {
    my @a = 1, 2, 3;
    for 1, 2 { @a.map({ $_ = 5 }).eager; $_ = 9 }
}, X::AdHoc,
    message => 'Cannot assign to an immutable value',
    'the for-literal topic is immutable again after a nested writable binding';
