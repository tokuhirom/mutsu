use v6;
use Test;

# An END phaser's captured env is refreshed after every closure call whose own
# captured free variables merely SHARE A NAME with something the phaser
# captured -- `update_end_phaser_envs_for_keys` used to walk every registered
# phaser and overwrite its captured entry for any name-matching key, with no
# check that the two were the same binding. A closure created in a completely
# different scope (e.g. the mainline) that happens to capture a lexical with
# the same short name as one an END phaser captured from a now-dead block
# scope would clobber the phaser's frozen (and correct) value.
#
# The fix: `update_end_phaser_envs_for_keys` must not touch a key already in
# a phaser's `dead_keys` -- that set already records which captured names are
# this phaser's own authoritative surviving binding (frozen when their
# declaring scope died), and a same-named capture from elsewhere must never
# override it.

plan 7;

my $dir = $*TMPDIR.child("mutsu-end-shadow-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim
}

# 1. Read side: a called closure only READS a same-named lexical of a
#    different binding. The phaser's own (dead-scope) binding must survive.
my $read-side = 'sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;
callit { $a };
';
is run-snippet('read-side.raku', $read-side), 'END1 (want 42): 42',
    'a called closure that only reads a same-named different binding does not clobber the phaser';

# 2. Write side: a called closure WRITES a same-named lexical of a different
#    binding (via a runtime-resolved name). The write must land on the live
#    mainline binding, not leak into the phaser's frozen one.
my $write-side = 'sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;
callit { $::(\'a\') = 7 };
';
is run-snippet('write-side.raku', $write-side), 'END1 (want 42): 42',
    'a called closure that writes a same-named different binding does not leak into the phaser';

# 3. Negative control: an END installed INSIDE the called closure (no capture
#    of a same-named outer lexical at all) must still work normally.
my $inner-end = 'sub callit(&c) { c() }
callit { END { 1 } };
say "control1 ok";
';
is run-snippet('inner-end.raku', $inner-end), 'control1 ok',
    'an END phaser installed inside a called closure runs fine';

# 4. Negative control: a called closure with no free-variable capture at all.
my $no-capture = 'sub callit(&c) { c() }
callit { 1 };
say "control2 ok";
';
is run-snippet('no-capture.raku', $no-capture), 'control2 ok',
    'a called closure with no captures at all is unaffected';

# 5. Multiple same-named dead-scope phasers must each keep their own binding,
#    unaffected by a later closure call that captures yet another same-named
#    live lexical.
my $two-dead = 'sub callit(&c) { c() }
{ my $a = 1; END { say "first a=$a" }; }
{ my $a = 2; END { say "second a=$a" }; }
my $a = 0;
callit { $a };
';
is run-snippet('two-dead.raku', $two-dead).lines.sort.join('|'),
    'first a=1|second a=2',
    'two same-named dead-scope END phasers each keep their own binding';

# 6. Positive control: the ORIGINAL propagation this refresh exists for still
#    works -- a closure that mutates the SAME (still-live) binding an END
#    phaser captured must be seen by the phaser.
my $same-binding = 'my $x = 1;
END { say "x=", $x }
my $c = { $x = 2 };
$c();
';
is run-snippet('same-binding.raku', $same-binding), 'x=2',
    'a closure mutating the SAME live binding an END captured still propagates';

# 7. Positive control variant: the mutating closure is called through another
#    sub (mirrors the read/write-side repros' call shape) rather than invoked
#    directly.
my $same-binding-called = 'sub callit(&c) { c() }
my $x = 1;
END { say "x=", $x }
callit { $x = 9 };
';
is run-snippet('same-binding-called.raku', $same-binding-called), 'x=9',
    'a called closure mutating the SAME live binding an END captured still propagates';
