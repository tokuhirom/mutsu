use Test;

# From Distribution::Extension::Updater (t/01-basic.rakutest):
# `open(:create)` with no write mode creates the file and returns a read handle.

plan 6;

my $dir = $*TMPDIR.add("mutsu-open-create-{$*PID}-{(^100000).pick}");
$dir.mkdir;
LEAVE { for $dir.dir { .unlink }; $dir.rmdir }

my $f = $dir.add("a.txt");
my $fh = $f.open(:create);
isa-ok $fh, IO::Handle, ':create alone yields a handle';
$fh.close;
ok $f.e, 'file was created';
is $f.s, 0, 'file is empty';

my $g = $dir.add("b.txt");
$g.open(:create, :exclusive).close;
ok $g.e, ':create, :exclusive creates the file';
dies-ok { $g.open(:create, :exclusive) }, ':exclusive fails on an existing file';

$f.spurt("keep");
$f.open(:create).close;
is $f.slurp, "keep", ':create does not truncate an existing file';
