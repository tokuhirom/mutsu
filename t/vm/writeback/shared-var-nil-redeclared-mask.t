use Test;

plan 6;

# The cross-thread shared store is keyed by BARE NAME and is global to the
# process: `clone_for_thread` migrates every lexical it can see into it on each
# `start`/Proc::Async spawn. A later, unrelated frame that declares a lexical of
# the same name must NOT see that foreign entry — its own binding shadows it.
# The GetLocal read path used to consult the shared store whenever a slot held
# Nil (treating Nil as "uninitialized, refresh from the store"), so a `:=` bind
# that legitimately yielded Nil resurrected the other scope's value.

sub nil-returner() { Nil }

sub spawner() {
    my $depends = True;
    await start { $depends };
}

sub victim-bind() {
    my $depends := nil-returner();
    $depends;
}

sub victim-assign() {
    my $depends = nil-returner();
    $depends;
}

# The assign case is only checked for definedness: `my $x = Nil` resets the
# container to its default, so raku yields Any where mutsu yields Nil. Either is
# undefined; the leak this file guards against shows up as a defined True.
is victim-bind(), Nil, 'bind of a Nil-returning call is Nil before any spawn';
nok victim-assign().defined, 'assign of a Nil-returning call is undefined before any spawn';

spawner();

# After the spawn, "depends" => True lives in the shared store, but it belongs to
# spawner's lexical, not to victim's freshly-declared one.
is victim-bind(), Nil, 'bind is still Nil after a same-named lexical was shared to a thread';
nok victim-assign().defined,
    'assign is still undefined after a same-named lexical was shared to a thread';

# The mask must not break genuine cross-thread sharing: a lexical the spawning
# frame itself shares still observes the child thread's write.
sub shares-back() {
    my $counter = 0;
    await start { $counter = 42 };
    $counter;
}
is shares-back(), 42, 'a thread write to the spawning frame own lexical is still visible';

# A Nil-bound lexical also stays Nil when a thread is spawned between the
# declaration and the read.
sub victim-spawn-between() {
    my $depends := nil-returner();
    await start { 1 };
    $depends;
}
is victim-spawn-between(), Nil, 'bind stays Nil across a spawn between declaration and read';
