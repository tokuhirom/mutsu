use Test;

# Tier-1 state-only IO::Handle methods (chomp / nl-out / out-buffer / encoding /
# native-descriptor) — resolved VM-native via the shared IoHandleState methods
# (③ native IO PR-D Tier-1). These touch only the handle's own state, so they
# must behave identically to the interpreter's native fork.

plan 14;

my $path = $*TMPDIR.child("mutsu-io-tier1-{$*PID}.txt");
$path.spurt("hello\nworld\n");

my $fh = $path.open(:w);

# .chomp getter/setter (rw accessor)
ok $fh.chomp ~~ Bool, '.chomp returns a Bool';
ok $fh.chomp, '.chomp defaults to True';
$fh.chomp = False;
nok $fh.chomp, '.chomp = False updates the setting';
$fh.chomp = True;
ok $fh.chomp, '.chomp = True restores it';

# .nl-out getter/setter
is $fh.nl-out, "\n", '.nl-out defaults to "\n"';
$fh.nl-out = "\r\n";
is $fh.nl-out, "\r\n", '.nl-out = updates the output terminator';

# .encoding getter/setter (method-call form)
ok $fh.encoding.defined, '.encoding returns a defined value';
# (the setter return and a follow-up getter agree on the resolved name; mutsu
# keeps it verbatim, raku normalizes it — assert consistency rather than a name)
my $enc = $fh.encoding('latin1');
is $fh.encoding, $enc, '.encoding(name) switches and reads back consistently';

# .out-buffer getter/setter
ok $fh.out-buffer ~~ Int, '.out-buffer returns an Int';
$fh.out-buffer = 4096;
is $fh.out-buffer, 4096, '.out-buffer = sets the capacity';

# .native-descriptor returns a real fd on a file handle
ok $fh.native-descriptor >= 0, 'file handle exposes a native descriptor';

$fh.close;

# standard handles map to the conventional fds
is $*IN.native-descriptor, 0, '$*IN native-descriptor is 0';
is $*OUT.native-descriptor, 1, '$*OUT native-descriptor is 1';
is $*ERR.native-descriptor, 2, '$*ERR native-descriptor is 2';

$path.unlink;
