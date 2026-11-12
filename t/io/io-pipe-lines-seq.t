use Test;

# `.lines` on a process's output pipe is a Seq, as on any IO::Handle, so a
# `--> Seq` routine can return it (Archive::SimpleZip's t/ZipTest.rakumod).

plan 4;

my $proc = run $*EXECUTABLE, '-e', 'say "a"; say "b"', :out;
my $lines = $proc.out.lines(:chomp);
isa-ok $lines, Seq, 'IO::Pipe.lines is a Seq';
is $lines.list, <a b>, 'with the lines';

sub lines-of(--> Seq) { run($*EXECUTABLE, '-e', 'say "x"', :out).out.lines }
is lines-of().list, <x>, 'a --> Seq routine can return it';

my $p2 = run $*EXECUTABLE, '-e', 'say "1"; say "2"', :out;
my $out = $p2.out;
$out.get;
isa-ok $out.lines, Seq, 'the rest after a .get is a Seq too';
