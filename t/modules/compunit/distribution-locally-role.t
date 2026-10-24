use v6;
use Test;

# `Distribution::Locally` is the role that gives a distribution a `prefix` and
# a `content` that hands back an UNOPENED handle on the file under it
# (mutsu#14906, Pakku::Meta's `self does Distribution::Locally(:$prefix)`).
plan 8;

my $dir = $*TMPDIR.add( 'locally-' ~ $*PID );
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.add('res').mkdir;
$dir.add('res/x.txt').spurt: "hello\n";

class D does Distribution::Locally {
  method meta { { name => 'D', files => { 'resources/x.txt' => 'res/x.txt' } } }
}

my $d = D.new( prefix => $dir );
is $d.prefix.absolute, $dir.absolute, 'the prefix attribute';
my $mapped = $d.content('resources/x.txt');
isa-ok $mapped, IO::Handle, 'content is an IO::Handle';
nok $mapped.opened, 'which is not open yet';
is $mapped.path.basename, 'x.txt', 'the files map resolves the address';
is $d.content('res/x.txt').path.basename, 'x.txt', 'an address outside the map is used as written';
$mapped.open;
is $mapped.slurp.chomp, 'hello', 'and reads the file once opened';

class E {
  method meta { { files => {} } }
  method content($a) { 1 }
}
my $e = E.new;
$e does Distribution::Locally( prefix => $dir );
is $e.prefix.absolute, $dir.absolute, 'mixing the role in with a prefix argument';
ok $e ~~ Distribution::Locally, 'and the mixin type-checks';
