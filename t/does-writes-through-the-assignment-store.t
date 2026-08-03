use v6;
use Test;

# `$x does R` rebinds `$x` exactly as `$x = ...` does, so it has to use the same
# by-name store. It used a raw `env.insert` instead, which skips the redirect a
# compunit's own file-scope `my` needs: such a name lives in the compunit's own
# cell while one of its routines is running, NOT under the bare env key (which
# belongs to the scope that loaded the module). The mixin therefore landed on a
# key nothing read back, and the module's own `$state` stayed un-mixed.

plan 4;

my $dir = $*TMPDIR.child("mutsu-does-store-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

$dir.child('DoesStore.rakumod').spurt(
    'unit module DoesStore;
role R { has $.tag = "tagged" }
my $state = {:x};
my $plain = 1;
sub mixin-it() is export { $state does R }
sub read-it() is export { (try $state.tag) // "LOST" }
sub mixin-plain() is export { $plain does R }
sub read-plain() is export { (try $plain.tag) // "LOST" }
');

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim.subst("\n", " ", :g)
}

my $lib = 'use lib "' ~ $dir.absolute ~ '";' ~ "\n";

is run-snippet('hash.raku', $lib ~ 'use DoesStore;
mixin-it();
say read-it();
'), 'tagged', 'a `does` on a compunit file-scope lexical is visible to the module';

is run-snippet('int.raku', $lib ~ 'use DoesStore;
mixin-plain();
say read-plain();
'), 'tagged', 'and the same holds for a scalar-valued one';

# Not a regression of the ordinary shapes.
role Local { has $.k = 7 }
my $a = {:x};
$a does Local;
is $a.k, 7, 'a `does` at file scope still works';

my $b = {:x};
{ $b does Local }
is $b.k, 7, 'and inside a bare block';
