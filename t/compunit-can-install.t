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
# installable.
my $unrootable = CompUnit::Repository::Installation.new(
    prefix => "/mutsu-nonexistent-root-{$*PID}/repo");
nok $unrootable.can-install, 'prefix under a non-writable root cannot install';
