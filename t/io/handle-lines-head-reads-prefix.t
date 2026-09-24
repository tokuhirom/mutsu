use Test;

# `.head(n)` / `.first` on an `IO::Handle.lines` Seq read only the lines they
# return, as Rakudo's line iterator does: the handle is left positioned on the
# next line instead of being drained to EOF.

plan 8;

my $path = $*TMPDIR.add("mutsu-handle-lines-head-$*PID.txt");
$path.spurt: "l1\nl2\nl3\nl4\n";
LEAVE $path.unlink;

{
    my $fh = open $path;
    is-deeply $fh.lines.head(2), ("l1", "l2").Seq, '.lines.head(2)';
    is $fh.get, "l3", '... leaves the handle on the third line';
    $fh.close;
}
{
    my $fh = open $path;
    is $fh.lines.first, "l1", '.lines.first';
    is $fh.get, "l2", '... leaves the handle on the second line';
    $fh.close;
}
{
    my $fh = open $path;
    is-deeply $fh.lines.head(10), ("l1", "l2", "l3", "l4").Seq, '.head past the end';
    is $fh.get, Nil, '... reads to EOF';
    $fh.close;
}
{
    my $fh = open $path;
    my $lines = $fh.lines;
    $lines.head(1);
    throws-like { $lines.List }, X::Seq::Consumed, '.head consumes the Seq';
    $fh.close;
}
{
    my $fh = open $path;
    is-deeply $fh.lines.List, ("l1", "l2", "l3", "l4"), 'a full read is unchanged';
    $fh.close;
}
