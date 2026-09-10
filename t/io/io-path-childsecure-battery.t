use Test;

# IO::Path::ChildSecure is bundled (BATTERIES.md §7,
# docs/batteries/io-path-childsecure.md), so it must load and work with a
# plain `use` -- no `-I`, no install. This pins the zero-config resolution and
# a smoke slice of the API; the exhaustive behaviour check is the release-time
# gate that runs the full upstream suite (scripts/battery-testsuite.sh).

plan 4;

use IO::Path::ChildSecure;

my $dir = $*TMPDIR.child("mutsu-childsecure-{$*PID}");
$dir.mkdir;
END { rmdir $dir }

my $kid = $dir.&child-secure('meow');
isa-ok $kid, IO::Path, 'a proper child resolves to an IO::Path';
is $kid.basename, 'meow', 'the child keeps its name';

my $escape = $dir.&child-secure('../escape');
isa-ok $escape, Failure, 'an escaping path yields a Failure';
throws-like { $escape.sink }, X::IO::NotAChild, 'the Failure carries X::IO::NotAChild';
