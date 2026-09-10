use v6;
use Test;
use nqp;

# The process/introspection `nqp::` ops. Rakudo's own `lib/Test.rakumod` is
# written against exactly these (plus the arithmetic ones mutsu already had),
# so they are what stands between mutsu and running the genuine upstream Test
# module instead of its native reimplementation — see
# todo/tickets/vendor-real-test-module.md.
#
# `can`, `join`, `split` and `time` all collide with same-named Raku builtins
# of DIFFERENT semantics, so each one is checked here against its nqp meaning,
# not its Raku namesake's.

plan 25;

# -- nqp::time: integer nanoseconds since the epoch --------------------------

my $t = nqp::time;
isa-ok $t, Int, 'nqp::time returns an Int';
ok $t > 1_700_000_000_000_000_000, 'nqp::time counts NANOseconds since the epoch';
ok nqp::time() >= $t, 'nqp::time is monotone across two readings';

# Written without parentheses all over Test.rakumod, so the no-paren term form
# has to reach the op rather than a qualified-symbol lookup.
my $bare = nqp::time;
isa-ok $bare, Int, 'the no-paren `nqp::time` term form works';

# -- nqp::getstdout / getstderr / getstdin + setbuffersizefh -----------------

# MoarVM hands back an opaque `BOOTIO`; mutsu has no such type and uses the
# same `IO::Handle` its own `$*OUT` is, so these three assertions are
# deliberately mutsu-specific. Everything a caller does with the result —
# `setbuffersizefh` here, `.print`/`.say` elsewhere — works either way.
my $out = nqp::getstdout();
isa-ok $out, IO::Handle, 'nqp::getstdout returns a handle';
isa-ok nqp::getstderr(), IO::Handle, 'nqp::getstderr returns a handle';
isa-ok nqp::getstdin(), IO::Handle, 'nqp::getstdin returns a handle';
is nqp::getstdout().native-descriptor, 1, 'nqp::getstdout is the process stdout (fd 1)';
is nqp::getstderr().native-descriptor, 2, 'nqp::getstderr is the process stderr (fd 2)';

# These are the PROCESS streams, deliberately not the dynamic variables: nqp
# code reaches for them precisely to bypass an override.
{
    my $*OUT = class { method print(|c) { } };
    is nqp::getstdout().native-descriptor, 1,
        'nqp::getstdout is the process stdout even when $*OUT is rebound';
}

# What Test.rakumod does with them: unbuffer, so TAP output cannot be reordered.
nqp::setbuffersizefh(nqp::getstdout(), 0);
is nqp::getstdout().out-buffer, 0, 'nqp::setbuffersizefh(fh, 0) unbuffers the handle';
nqp::setbuffersizefh(nqp::getstdout(), 4096);
is nqp::getstdout().out-buffer, 4096, 'and a non-zero size sets that capacity';
nqp::setbuffersizefh(nqp::getstdout(), 0);

# -- nqp::eqaddr: object identity --------------------------------------------

is nqp::eqaddr(Int, Int), 1, 'nqp::eqaddr is 1 for the same type object';
is nqp::eqaddr(Int, Str), 0, 'and 0 for different type objects';
is nqp::eqaddr(Mu, Mu), 1, 'nqp::eqaddr(Mu, Mu) — the shape `is` uses';
my @a;
is nqp::eqaddr(@a, @a), 1, 'the same container is identical to itself';
is nqp::eqaddr([1], [1]), 0, 'two equal-but-distinct containers are not';

# -- nqp::can: does this object have that method -----------------------------

is nqp::can(1, 'raku'), 1, 'nqp::can finds a method the object has';
is nqp::can(1, 'no-such-method-here'), 0, 'and answers 0 for one it does not';
class NqpCanTarget { method mine() { 42 } }
is nqp::can(NqpCanTarget.new, 'mine'), 1, 'nqp::can sees a user-defined method';
is nqp::can(NqpCanTarget.new, 'yours'), 0, 'and not one that was never defined';

# -- nqp::join / nqp::split: literal, over an nqp list ------------------------

is nqp::join('-', nqp::split(',', 'a,b,c')), 'a-b-c', 'nqp::split then nqp::join round-trips';
is nqp::join('|', nqp::split('', 'abc')), 'a|b|c', 'an empty separator splits into characters';
is nqp::join('|', nqp::split("\n", "a\n")), 'a|', 'a trailing separator keeps the empty field';
# The exact `diag` shape from Test.rakumod: indent every line of a message.
is nqp::join("\n# ", nqp::split("\n", "one\ntwo")), "one\n# two",
    'the `diag` indentation idiom from Test.rakumod';
