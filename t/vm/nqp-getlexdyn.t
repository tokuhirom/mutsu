use v6;
use Test;
use nqp;

# `nqp::getlexdyn($name)` was entirely unimplemented ("Unsupported nqp:: op:
# nqp::getlexdyn"), which blocked Rakudo::Options
# (`nqp::atkey(nqp::getlexdyn('%*COMPILING'),'%?OPTIONS')`). Issue #8572.
#
# Real NQP `getlexdyn` only resolves names actually declared as a *lexical*
# dynamic somewhere in the caller chain -- confirmed against `raku` itself:
# `%*ENV`/`$*OUT`/`@*ARGS`/... live in `PROCESS::` and do NOT resolve through
# it (they come back `VMNull`), while `%*COMPILING` (declared `my %*COMPILING`
# by the compiler itself) and an ordinary `my $*x = ...` dynamic do. mutsu's
# implementation is a superset of that: it resolves through the same
# chokepoint (`get_env_with_main_alias`) every ordinary dynamic read goes
# through, rather than replicating NQP's lexpad-vs-`PROCESS::` distinction,
# which mutsu has no equivalent of. `%*COMPILING` (this issue's actual
# target) and user-declared dynamics behave identically to raku either way.

plan 5;

# %*COMPILING<%?OPTIONS> is Rakudo::Options's own access pattern
# (`nqp::atkey(nqp::getlexdyn('%*COMPILING'),'%?OPTIONS')`); it must be
# Hash-shaped and carry at least `encoding`, even with no CLI flags in play
# (an ordinary `.t` run passes the file as an argument, not `-e`).
my $compiling := nqp::getlexdyn('%*COMPILING');
isa-ok $compiling, Hash, '%*COMPILING is a Hash';
my $options := nqp::atkey($compiling, '%?OPTIONS');
isa-ok $options, Hash, '%*COMPILING<%?OPTIONS> is a Hash';
is $options<encoding>, 'utf8',
    '%*COMPILING<%?OPTIONS><encoding> defaults to utf8';

# A user-declared dynamic in an enclosing (caller) frame is visible too --
# this is not special-cased to %*COMPILING alone.
sub with-dynamic() {
    my $*CUSTOM-DYNAMIC = 'hello';
    inner-getlexdyn();
}
sub inner-getlexdyn() {
    nqp::getlexdyn('$*CUSTOM-DYNAMIC');
}
is with-dynamic(), 'hello',
    'getlexdyn resolves a user-declared dynamic walking the caller chain';

# An unknown dynamic name is a hard error in mutsu -- deliberately, not a
# reproduction of NQP's own VMNull (a raw ops-level null with no Raku-level
# equivalent that mutsu does not model): a clear error beats a value that
# only misbehaves later, for a case no real module exercises (a module only
# ever calls getlexdyn on a name it knows exists).
throws-like { nqp::getlexdyn('%*NO-SUCH-DYNAMIC-EVER') }, Exception,
    'getlexdyn on an unknown dynamic name throws';
