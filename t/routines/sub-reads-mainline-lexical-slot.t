use Test;

# A top-level or bare-block sub reads the unit-level lexicals it captured
# (ADR-0024) through a slot of its own frame (#18274). Every read must answer
# what a by-name read does.

plan 9;

my @out;
my %seen;
my $sep = ',';
my $count = 0;

sub record($v) {
    @out.push($sep) if @out;
    @out.push($v);
    %seen{$v}++;
    $count++;
    @out.elems ~ '/' ~ %seen.elems ~ '/' ~ $count
}
is record('a'), '1/1/1', 'first call reads and writes the unit lexicals';
is record('b'), '3/2/2', 'a second call sees the first call\'s writes';
$sep = ';';
record('c');
is @out.join, 'a,b;c', 'a mainline assignment is seen by the next call';

sub loop-read(int $n) { my $s = 0; for ^$n { $s += @out.elems + %seen.elems } ; $s }
is loop-read(3), 24, 'repeated reads in one call';

sub caller-shadow() { my @out = 'x'; record-len() }
sub record-len() { @out.elems }
is caller-shadow(), 5, 'a caller\'s same-named local does not shadow the capture';

{
    my $blk = 'b1';
    sub in-block() { $blk ~ $blk }
    is in-block(), 'b1b1', 'a bare-block sub reads its block\'s lexical';
    $blk = 'b2';
    is in-block(), 'b2b2', 'and sees a later assignment';
}

sub rec($n) { $n == 0 ?? $count !! rec($n - 1) + $count }
is rec(2), 9, 'recursion reads the capture in every frame';

@out = ();
is record-len(), 0, 'a whole-array assignment is seen';
