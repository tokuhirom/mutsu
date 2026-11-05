use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 3;

# From XDG::GuaranteedResources: `copy(%?RESOURCES<file>, $dest)` copies the
# file the resource names (the routine form used to stringify the resource).
use ResSplit;

my $r = strs();
my $dest = $*TMPDIR.child("mutsu-copy-resource-{$*PID}");
LEAVE $dest.unlink if $dest.e;

ok copy($r, $dest), 'copy of a resource succeeds';
is $dest.slurp, $r.slurp, 'the destination has the resource content';
$dest.unlink;
nok copy($r, $dest, :createonly) ?? False !! True, 'createonly copies when the destination is absent';
