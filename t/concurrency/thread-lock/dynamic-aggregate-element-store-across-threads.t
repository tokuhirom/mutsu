use v6;
use Test;

plan 8;

# Once any thread has run, an element store into a shared `@`/`%` goes through
# the cross-thread store. The owning frame's read consults the `__mutsu_atomic_*`
# lane for every `@`/`%` name, but a dynamic `%*h` / `@*a` element store went to
# the base-key entry instead, so the owner never saw it (#13772: roast
# S17-promise/then.t's `%*FOO` came back `{}`).

{
    my %*H;
    await start { 1 };
    my &store = { %*H<c> = 1 };
    store();
    is-deeply %*H, {:c(1)}, 'closure stores into %*H after a spawn';
}

{
    my @*A;
    await start { 1 };
    my &store = { @*A[0] = 1 };
    store();
    is-deeply @*A, [1], 'closure stores into @*A after a spawn';
}

{
    my %*H;
    my $p = start { %*H<prom> = 1 };
    await $p;
    await $p.then: { %*H<then> = 2 };
    is-deeply %*H, {:prom(1), :then(2)}, 'inline .then on a kept start stores into %*H';
}

{
    my @*A;
    my $p = start { @*A[0] = 1 };
    await $p;
    await $p.then: { @*A[1] = 2 };
    is-deeply @*A, [1, 2], 'inline .then on a kept start stores into @*A';
}

# Each iteration's `my %*H` is a fresh binding; none may see another's stores.
my $lost = 0;
for ^20 -> $i {
    my %*H;
    await start { %*H<prom> = $i }.then: { %*H<then> = $i };
    $lost++ unless %*H eqv {:prom($i), :then($i)};
}
is $lost, 0, 'start + .then stores into %*H survive every iteration';

$lost = 0;
for ^20 -> $i {
    my %*H;
    await Promise.kept(1).then: { %*H<then> = $i };
    $lost++ unless %*H eqv {:then($i)};
}
is $lost, 0, '.then on an already-kept promise stores into a fresh %*H';

{
    my %*H;
    await start { %*H<a> = 1 };
    is-deeply %*H, {:a(1)}, 'a start block stores into %*H';
}

{
    my %*H;
    await start { is-deeply %*H, {}, 'a later %*H does not see an earlier binding' };
}
