use v6;
use Test;

# ADR-0086 / #7557: the built-in dynamic variables ($*OUT, $*CWD, %*ENV,
# @*ARGS, $*REPO, ...) live in a per-interpreter never-copied base tier rather
# than in every env's own map, so a closure capture no longer rebuilds ~20
# entries the call-time merge would discard anyway. This pins the behaviour
# that move must not change: they are still readable, writable, shadowable,
# restorable and visible to the pseudo-stashes.

plan 18;

# --- plain reads, through a closure and through a callee -------------------
ok $*CWD.defined, '$*CWD reads from the base tier';
ok $*TMPDIR.defined, '$*TMPDIR reads from the base tier';
ok %*ENV.defined, '%*ENV reads from the base tier';
ok @*ARGS.defined, '@*ARGS reads from the base tier';

my $in-closure = { $*CWD.defined };
ok $in-closure(), 'a closure created at mainline still sees $*CWD';

sub sees-cwd() { $*CWD.defined }
ok sees-cwd(), 'a sub with no enclosing binding still sees $*CWD';

sub makes-closure() { return { $*TMPDIR.defined } }
ok makes-closure()(), 'a closure created inside a sub frame still sees $*TMPDIR';

# --- a user binding shadows the base and is what actually gets captured ----
{
    my $*CWD = "/nonesuch-shadow".IO;
    is $*CWD.Str, '/nonesuch-shadow', 'my $*CWD shadows the base tier';
    my $inner = { $*CWD.Str };
    is $inner(), '/nonesuch-shadow', 'the shadowing binding is what a closure captures';
}
isnt $*CWD.Str, '/nonesuch-shadow', 'the shadow is gone once its block exits';

# --- assignment is promoted out of the base and persists ------------------
{
    my $saved = $*CWD;
    sub set-cwd($p) { $*CWD = $p }
    set-cwd("/".IO);
    is $*CWD.Str, '/', 'assigning $*CWD inside a sub is visible after it returns';
    $*CWD = $saved;
    is $*CWD.Str, $saved.Str, '$*CWD restores to its seeded value';
}

# --- %*ENV stays mutable ---------------------------------------------------
%*ENV<MUTSU_BASE_TIER_PROBE> = 'set';
is %*ENV<MUTSU_BASE_TIER_PROBE>, 'set', '%*ENV is still writable through the base tier';

# --- the pseudo-stashes still see them ------------------------------------
ok PROCESS::<$OUT>.defined, 'PROCESS::<$OUT> still finds the seeded handle';
my $*MUTSU-BASE-PROBE = 'bound';
is DYNAMIC::<$*MUTSU-BASE-PROBE>, 'bound', 'DYNAMIC:: still finds a user-bound dynamic';
sub stash-from-callee() { PROCESS::<$CWD>.defined }
ok stash-from-callee(), 'PROCESS::<$CWD> resolves from inside a callee too';

# --- output still routes through a redirected $*OUT -----------------------
my $captured = '';
{
    my $*OUT = class { method print(*@a) { $captured ~= @a.join }; method flush { } }.new;
    say 'redirected';
}
is $captured, "redirected\n", 'a lexically redirected $*OUT still wins over the base tier';

# --- and through a thread clone -------------------------------------------
my $threaded = '';
{
    my $*OUT = class { method print(*@a) { $threaded ~= @a.join }; method flush { } }.new;
    await start { print 'from-thread' };
}
is $threaded, 'from-thread', 'a start block inherits the redirected $*OUT';
