use Test;

# ADR-0055: an escaping capture the creating frame cannot vouch for gets a
# shared `ContainerRef` cell. When the captured name is the frame's own
# PARAMETER, that cell must stay a per-invocation binding: it must NOT be
# published into the cross-thread `shared_vars` lane, which is keyed by BARE
# NAME and is process-wide.
#
# Publishing it declares "this cell is what the name means" for every frame in
# the program, so a callee's parameter hijacks an unrelated same-named lexical
# in its caller. That is what dropped six Cro::HTTP suites: the test script's
# `my $url = 'http://localhost:PORT'` was overwritten by
# `Cro::HTTP::Client.request`'s `$url` parameter, so every request after the
# second one asked for an accumulating path (`/b/a`, `/b/a/b`, ...) and 404'd.
#
# Three ingredients are all load-bearing:
#   * a thread must be running, or the lane is inactive;
#   * the spawned block must NOT mention the caller's lexical -- a scalar the
#     block captures is excluded from the lane's seeding, and the leak travels
#     on a seeded entry;
#   * the callee's parameter must be BOTH handed to a call (so the frame
#     refuses to vouch for it) and captured by an escaping closure (so it is
#     boxed at all).

plan 6;

sub noop($v) { 1 }

{
    my @kept;
    sub client-a($url) { noop($url); @kept.push({ $url }); }
    my $url = "BASE";
    my $worker = start { sleep 0.2; 1 };
    for <a b a b> -> $x { client-a("$url/$x") }
    await $worker;
    is $url, 'BASE',
        "a callee's captured parameter does not overwrite the caller's same-named lexical";
    is @kept.map({ .() }).join(','), 'BASE/a,BASE/b,BASE/a,BASE/b',
        '... and each stored closure still reads its own invocation\'s argument';
}

{
    # Same, with the caller's lexical forced into `env` by its own closure.
    my @kept;
    sub client-b($url) { noop($url); @kept.push({ $url }); }
    my $url = "BASE";
    my $g = { $url };
    $g.();
    my $worker = start { sleep 0.2; 1 };
    for <a b a b> -> $x { client-b("$url/$x") }
    await $worker;
    is $url, 'BASE', '... also when the caller\'s lexical is env-resident';
    is $g.(), 'BASE', '... and the caller\'s own closure still sees its binding';
}

{
    # A DECLARED lexical (not a parameter) keeps the lane: a worker must still
    # observe the parent's later write to a name it shares by name.
    my $counter = 0;
    my $worker = start { sleep 0.2; 1 };
    my $bump = { $counter = $counter + 1 };
    $bump.(); $bump.();
    await $worker;
    is $counter, 2, 'a declared lexical mutated through an escaping closure is still live';
}

{
    # And a parameter captured across a real thread boundary still reads its
    # own invocation's value on the worker.
    my @promises;
    sub spawn-with($tag) { noop($tag); start { $tag } }
    for <p q r> -> $t { @promises.push(spawn-with($t)) }
    is (await @promises).join(','), 'p,q,r',
        'a parameter captured by a spawned block keeps its own invocation\'s value';
}

# vim: expandtab shiftwidth=4
