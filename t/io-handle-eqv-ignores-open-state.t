use Test;

# Two independently-opened `IO::Handle`s for the same file are logically equal
# (same path/chomp/nl-in/nl-out/encoding) even though each `.open()` call
# allocates its own internal file-descriptor-registry id and mode -- verified
# against real Rakudo, whose own `IO::CatHandle.new(...)` internally re-opens
# an un-opened `IO::Handle` source, and `is-deeply $cat.raku.EVAL, $cat` still
# holds (roast S32-io/io-cathandle.t "perl method"). `eqv`/`is-deeply` must
# therefore ignore that internal open state, comparing only the public,
# reproducible attributes.

plan 4;

my $path = "tmp/io-handle-eqv-test.txt".IO;
$path.spurt("hello\n");

my $h1 = $path.open(:r);
my $h2 = $path.open(:r);
ok $h1 eqv $h2, 'two independent opens of the same file are eqv';

$h1.close;
$h2.close;

# `.raku.EVAL` reconstructs an *unopened* handle (no file-descriptor state);
# it must still be eqv to the originally-opened one.
my $h3 = $path.open(:r);
my $h4 = $h3.raku.EVAL;
ok $h3 eqv $h4, 'an opened handle is eqv to its unopened .raku.EVAL reconstruction';
$h3.close;

# A handle opened for writing is NOT eqv to one opened for reading with
# otherwise-identical settings applied afterward -- only chomp/nl-in/nl-out/
# encoding/path are compared, mode is deliberately excluded, but a genuinely
# different `path` must still make them unequal.
my $other = "tmp/io-handle-eqv-test-other.txt".IO;
$other.spurt("world\n");
my $h5 = $other.open(:r);
nok $h1 eqv $h5, 'handles to different files are not eqv';
$h5.close;

my $h6 = $path.open(:r, :!chomp);
my $h7 = $path.open(:r, :chomp);
nok $h6 eqv $h7, 'handles with different chomp settings are not eqv';
$h6.close;
$h7.close;
