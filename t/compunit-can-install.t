use v6;
use Test;

# CompUnit::Repository::Installation.can-install mirrors Rakudo: an install target
# is installable if the prefix is writable, OR it does not yet exist and can be
# *created* — i.e. the nearest already-existing ancestor directory is writable.
# zef's `install` command greps repos by `.?can-install` to choose an install
# target (the `auto` target picks the first of `site`/`home` that can-install), so
# getting this wrong stalls the whole install pipeline with "Need a valid
# installation target to continue".

plan 4;

my $base = $*TMPDIR.add("can-install-{$*PID}");
$base.mkdir;
# Instantiating an Installation / probing can-install may populate the prefix, so
# clean up recursively and ignore failures.
LEAVE { try { .unlink for $base.dir(:recursive).grep(*.f); $base.dir(:recursive).grep(*.d).reverse.map(*.rmdir); $base.rmdir } }

my $writable = CompUnit::Repository::Installation.new(prefix => $base.absolute);
ok $writable.can-install, 'writable existing prefix can-install';

my $child = CompUnit::Repository::Installation.new(prefix => $base.add("newchild").absolute);
ok $child.can-install, 'missing prefix under a writable parent can-install';

# Several missing path components at once: still installable, because the nearest
# existing ancestor ($base) is writable and the whole chain can be created. (This
# is exactly the shape of mutsu's default `~/.local/share/mutsu/repo/home` target
# on a fresh install.) raku agrees: such a prefix reports can-install=True.
my $deep = CompUnit::Repository::Installation.new(prefix => $base.add("a/b/c").absolute);
ok $deep.can-install, 'deeply-nested creatable prefix (missing ancestors) can-install';

# But a prefix whose only existing ancestor is a non-writable root is NOT
# installable. Don't hardcode "/" as that root: in some dev/container
# environments the invoking user actually owns "/" (e.g. a single-user LXC
# container where the process uid is also the owner of the filesystem root),
# which makes "/" genuinely writable and defeats the assumption this subtest
# relies on. Instead, probe a short list of directories that are reliably
# non-writable regardless of who owns the box -- /proc's top level is a
# synthetic filesystem that refuses new entries, and /sys is the same -- using
# the exact writability check `can-install` itself uses internally
# (`IO::Path.w`), and skip the subtest if none of them turns out to be
# non-writable in the current environment.
my $nonwritable-root;
for "/proc".IO, "/sys".IO, "/root".IO -> $candidate {
    next unless $candidate.e;
    if !$candidate.w {
        $nonwritable-root = $candidate;
        last;
    }
}

with $nonwritable-root -> $root {
    my $unrootable = CompUnit::Repository::Installation.new(
        prefix => $root.add("mutsu-nonexistent-root-{$*PID}/repo").absolute);
    nok $unrootable.can-install, 'prefix under a non-writable root cannot install';
}
else {
    skip 'no non-writable candidate root (/proc, /sys, /root) found in this environment', 1;
}
