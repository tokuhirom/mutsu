use Test;

plan 8;

# ADR-0010: the cross-thread store is scoped to a spawn lineage, not the process.
# It used to be one process-global map keyed by bare name, so sibling threads
# that each declared a same-named lexical raced on one entry (last writer won).

sub matcher($u) { 1 }

# Each hyper worker declares `my $uri` and then runs a nested `start` — that
# inner spawn migrates the worker's lexicals into the store a second time, which
# is what made N workers collide on the one `uri` key.
sub fetch($candi) {
    my $uri = "u-$candi";
    matcher($uri);
    my &code = -> {
        my $todo = start { "$candi:$uri" };
        await $todo;
        $todo.result;
    };
    code();
}
my @got = <A B C D E F>.hyper(:batch(1), :degree(5)).map: -> $c { fetch($c) };
is-deeply @got.sort.List, <A:u-A B:u-B C:u-C D:u-D E:u-E F:u-F>.List,
    'sibling hyper workers each keep their own same-named lexical';

# The same shape with the closure passed to a callee (how zef writes it: the
# block goes to lock-file-protect). This used to lose `$todo` itself to Any.
sub callee($path, &code) { code() }
sub fetch2($candi) {
    my $uri = "v-$candi";
    callee("$candi.lock", -> {
        my $todo = start { "$candi:$uri" };
        await $todo;
        $todo.result;
    });
}
my @got2 = <A B C D>.hyper(:batch(1), :degree(4)).map: -> $c { fetch2($c) };
is-deeply @got2.sort.List, <A:v-A B:v-B C:v-C D:v-D>.List,
    'same, with the closure invoked through a callee';

# The sharing that must survive: a child writing the parent's lexical.
sub shares-back() {
    my $counter = 0;
    await start { $counter = 42 };
    $counter;
}
is shares-back(), 42, 'a child thread write to the spawning frame lexical is visible';

# A `start` nested inside a worker must still see THAT worker's lexical, i.e.
# the chain to the parent lineage works (not just isolation from siblings).
sub nested-sees-own() {
    my $tag = 'outer';
    await start {
        await start { $tag };
    };
}
is nested-sees-own(), 'outer',
    'a start nested inside a spawned thread still sees its own lineage lexical';

# Internal keys are NOT lexicals: atomics are process-wide by design and must
# not be lineage-scoped (that would give each thread its own counter).
my atomicint $hits = 0;
await (^4).map: { start { $hits⚛++ for ^250 } };
is $hits, 1000, 'atomics stay process-wide shared across sibling threads';

# The other direction of shadowing: a child's `my` RE-declaration of a name its
# parent lineage owns is a fresh binding — its writes must NOT chain through to
# the parent (the chained write resolves to the nearest ancestor owning the
# name, so without the re-declaration mask this clobbered the parent's value).
# Regressed advent2013-day14.t: `if INIFile.parse(...) -> $parsed { }` inside a
# start block desugars to `my $parsed = ...` and replaced the parent's
# `my $parsed = Channel.new` with a Match.
sub child-my-shadows() {
    my $c = Channel.new;
    await start { my $c = 42; $c };
    $c;
}
isa-ok child-my-shadows(), Channel,
    'a child `my` re-declaration does not clobber the parent lexical';

sub child-pointy-shadows() {
    my $parsed = Channel.new;
    await start { if 42 -> $parsed { $parsed } };
    $parsed;
}
isa-ok child-pointy-shadows(), Channel,
    'a pointy-if binding in a child does not clobber the parent lexical';

# Within the child, the shadow is block-scoped: after the inner block exits the
# name must resolve to the captured parent value again (the store must not
# resurrect the dead shadow into the restored env).
sub child-shadow-block-exits() {
    my $c = 'parent';
    await start {
        { my $c = 'shadow'; }
        $c;
    };
}
is child-shadow-block-exits(), 'parent',
    'the child shadow dies at block exit; the captured outer value is back';
