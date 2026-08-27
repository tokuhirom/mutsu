use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# Regression test for two bugs in `chdir`'s adverb handling:
#
# 1. `chdir :!d, $path` took the `:!d` Pair itself as the target path
#    (stringified to something like "d\tFalse") instead of skipping it to
#    find the real positional path.
# 2. `:!d` did not actually skip the directory-existence test, so
#    `chdir :!d, $nonexistent` failed even though rakudo lets it succeed:
#    `$*CWD` is a *virtual* working directory, and `:d` (default True) is
#    what requests the existence+directory-ness test in the first place.
#
# Measured against rakudo (v2026.06): `chdir` NEVER issues a real OS-level
# `chdir(2)`, even to a real, existing directory -- only `$*CWD` (and the
# `:cwd` rakudo passes to spawned subprocesses) moves. `/proc/self/cwd`
# stays the interpreter's original directory the whole time. mutsu's
# `builtin_chdir` still attempts a best-effort real `set_current_dir` for
# real directories (existing behavior, unrelated to this bug), but it must
# never be attempted for a path that plain doesn't exist -- which is
# already naturally the case since the target can't `.is_dir()`.

my $orig-cwd = $*CWD;

my $missing1 = $*TMPDIR.child("mutsu-chdir-adverbs-missing-1-{$*PID}");
my $missing2 = $*TMPDIR.child("mutsu-chdir-adverbs-missing-2-{$*PID}");
my $missing3 = $*TMPDIR.child("mutsu-chdir-adverbs-missing-3-{$*PID}");

# --- 1. :!d before the positional path, target does not exist ---
{
    my $result = chdir :!d, $missing1;
    ok $result.defined, ':!d before the path: chdir succeeds against a nonexistent path';
    is $result.Str, $missing1.Str, ':!d before the path: returned IO::Path is the requested path, not the adverb';
    is $*CWD.Str, $missing1.Str, ':!d before the path: $*CWD updated to the (nonexistent) target';
    chdir $orig-cwd;
}

# --- 2. :!d after the positional path ---
{
    my $result = chdir $missing2, :!d;
    ok $result.defined, ':!d after the path: chdir succeeds against a nonexistent path';
    is $result.Str, $missing2.Str, ':!d after the path: returned IO::Path is the requested path';
    chdir $orig-cwd;
}

# --- 3. multiple adverbs alongside :!d, target still does not exist ---
{
    my $result = chdir :!d, :!r, $missing3;
    ok $result.defined, 'multiple adverbs (:!d, :!r) still resolve the real positional path';
    is $result.Str, $missing3.Str, 'multiple adverbs: returned path matches the requested (nonexistent) target';
    chdir $orig-cwd;
}

# --- 4. plain chdir (no adverbs) to a nonexistent path must still fail ---
{
    fails-like { chdir $missing1 }, X::IO::Chdir,
        'plain chdir with no adverbs still fails for a nonexistent path';
    is $*CWD.Str, $orig-cwd.Str, 'a failed chdir does not change $*CWD';
}

# --- 5. chdir to a real, existing directory succeeds and is restored ---
{
    my $real-dir = make-temp-dir;
    my $result = chdir $real-dir;
    ok $result.defined, 'chdir to a real existing directory succeeds';
    is $*CWD.Str, $real-dir.Str, '$*CWD reflects the real directory after chdir';
    chdir $orig-cwd;
}

# Make sure we really did restore $*CWD (and the real process cwd, since
# mutsu's chdir best-effort-mirrors it) so later tests in this run aren't
# affected.
is $*CWD.Str, $orig-cwd.Str, '$*CWD is restored to its original value at the end of the test';

done-testing;
