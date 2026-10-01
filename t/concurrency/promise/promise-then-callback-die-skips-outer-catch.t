use v6;
use Test;

# #10301: a `.then`/`.andthen`/`.orelse` on an already-resolved promise runs
# its callback synchronously, and a `die` in that callback used to run a
# `CATCH` around the chaining call inline (ADR-0072) — even though the failure
# only breaks the derived promise. The handler then ran a second time when the
# promise was awaited, so `throws-like { await $p.then({ die }) }` ran its two
# assertions twice (Cro::Core t/message-with-body.rakutest, whenever the
# body promise was kept before `.then` was called).

plan 7;

my $kept = Promise.kept(1);

{
    my $runs = 0;
    {
        await $kept.then({ die "boom" });
        CATCH { default { $runs++ } }
    }
    is $runs, 1, 'CATCH around await of a dying .then runs exactly once';
}

{
    my @seen;
    {
        my $p = $kept.then({ die "not here" });
        @seen.push: 'after .then';
        @seen.push: (try await $p) // $!.message;
        CATCH { default { @seen.push: "CATCH: $_" } }
    }
    is-deeply @seen, ['after .then', 'not here'],
        'a dying .then callback does not reach a CATCH around the .then call';
}

{
    my @seen;
    {
        my $p = $kept.andthen({ die "a" });
        @seen.push: (try await $p) // $!.message;
        CATCH { default { @seen.push: "CATCH: $_" } }
    }
    is-deeply @seen, ['a'], '.andthen callback failure stays in the promise';
}

{
    my @seen;
    {
        my $p = Promise.broken("b").orelse({ die "o" });
        @seen.push: (try await $p) // $!.message;
        CATCH { default { @seen.push: "CATCH: $_" } }
    }
    is-deeply @seen, ['o'], '.orelse callback failure stays in the promise';
}

{
    my $caught;
    {
        my $p = $kept.then({ die "inner" });
        try await $p;
        $caught = $!.message;
        CATCH { default { flunk "outer CATCH ran: $_" } }
    }
    is $caught, 'inner', 'try around await sees the callback failure';
}

class X::Test10301 is Exception { method message { 'custom' } }

# The shape of the Cro test: the subtest's plan must hold.
subtest {
    plan 2;
    throws-like { await $kept.then({ X::Test10301.new.throw }) }, X::Test10301,
        'throws-like over an awaited dying .then';
    pass 'still in plan';
}, 'throws-like subtest runs its assertions once';

pass 'reached the end';
