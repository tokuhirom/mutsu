use Test;

# Smart-matching an IO::Path against :r/:w/:x/:e must route to the
# corresponding IO::Path test method, so the file's permission bits are
# actually consulted.
#
# The assertions below are deliberately uid-independent. The original version
# of this test asserted `'/sys'.IO ~~ :w` is False, which only holds for an
# unprivileged user: root bypasses the DAC permission check (CAP_DAC_OVERRIDE),
# so access(2) reports /sys as writable and rakudo answers True there too. That
# made the file fail whenever the suite ran as root (the container/docker dev
# environment does). A negative that survives root needs a bit that root does
# not override -- the execute bit on a regular file with no x bits set -- or a
# path that does not exist at all.

plan 7;

my $dir = $*TMPDIR.add("mutsu-io-smartmatch-perm-{$*PID}");
mkdir $dir;
my $file = $dir.add('probe.txt');
$file.spurt('x');

is ($file ~~ :e), True,  'IO::Path smart-match against :e reports an existing file';
is ($file ~~ :r), True,  'IO::Path smart-match against :r reports a readable file';
is ($file ~~ :w), True,  'IO::Path smart-match against :w reports a writable file';

# No execute bit set anywhere: X_OK fails even for root, which needs at least
# one of the three x bits before it overrides the check.
$file.chmod(0o644);
is ($file ~~ :x), False, 'IO::Path smart-match against :x honours the missing execute bits';
$file.chmod(0o755);
is ($file ~~ :x), True,  'IO::Path smart-match against :x sees the execute bit';

my $missing = $dir.add('no-such-file');
is ($missing ~~ :w), False, 'IO::Path smart-match against :w is False for a missing path';

# Only meaningful unprivileged: root may write a mode 0444 file.
if +$*USER == 0 {
    skip 'running as root: W_OK is not denied by the permission bits', 1;
}
else {
    $file.chmod(0o444);
    is ($file ~~ :w), False, 'IO::Path smart-match against :w honours a read-only mode';
}

$file.chmod(0o644);
unlink $file;
rmdir $dir;
