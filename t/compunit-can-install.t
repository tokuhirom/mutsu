use v6;
use Test;

# CompUnit::Repository::Installation.can-install mirrors Rakudo: writable if the
# prefix is writable, or it does not yet exist and its parent is writable (so it
# can be created on install). zef's `install` command greps repos by
# `.?can-install` to choose an install target, so a missing method left the whole
# install pipeline stuck.

plan 3;

my $base = $*TMPDIR.add("can-install-{$*PID}");
$base.mkdir;
# Best-effort cleanup: instantiating an Installation may populate the prefix,
# so a plain rmdir can fail on a non-empty directory — ignore that.
LEAVE { try { $base.rmdir } }

my $writable = CompUnit::Repository::Installation.new(prefix => $base.absolute);
ok $writable.can-install, 'writable existing prefix can-install';

my $child = CompUnit::Repository::Installation.new(prefix => $base.add("newchild").absolute);
ok $child.can-install, 'missing prefix under a writable parent can-install';

my $deep = CompUnit::Repository::Installation.new(prefix => $base.add("a/b/c").absolute);
nok $deep.can-install, 'missing prefix whose parent also does not exist cannot-install';
