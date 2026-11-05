use v6;
use Test;

# `lines(:close)` closes the handle once the lines are exhausted, like
# `words(:close)`; a partial consumer leaves it open (#16128).

plan 8;

my $path = $*TMPDIR.add("lines-close-{$*PID}-{(^1000000).pick}.txt");
$path.spurt("a\nb\nc\n");
LEAVE try $path.unlink;

{
    my $h = $path.open(:r);
    my @m = $h.lines(:close);
    is @m.elems, 3, 'method form reads every line';
    nok $h.opened, 'method form closes at EOF';
}
{
    my $h = $path.open(:r);
    my @m = lines($h, :close);
    is @m.elems, 3, 'sub form reads every line';
    nok $h.opened, 'sub form closes at EOF';
}
{
    my $h = $path.open(:r);
    my @m = $h.lines(:close).map(*.uc);
    is @m.join, 'ABC', 'a stage over lines(:close)';
    nok $h.opened, 'closed after the stage is exhausted';
}
{
    my $h = $path.open(:r);
    my $first = $h.lines(:close).head(1).list;
    is $first.elems, 1, 'a partial read';
    ok $h.opened, 'a partial consumer leaves the handle open';
    $h.close;
}
