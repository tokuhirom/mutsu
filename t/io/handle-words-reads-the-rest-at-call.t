use Test;

# `IO::Handle.words` consumes the rest of the handle when called, as Rakudo's
# does; it does not stay lazy (#16012).
plan 9;

my $path = $*TMPDIR.add("handle-words-rest-{$*PID}.txt");
$path.spurt("alpha beta\ngamma delta\n");
LEAVE try $path.unlink;

{
    my $h = $path.open(:r);
    my $w = $h.words;
    is $h.tell, 23, '.words moves the handle to the end at once';
    ok $h.opened, 'and leaves it open';
    nok $w.is-lazy, 'the Seq is not lazy';
    is $w.List.raku, '("alpha", "beta", "gamma", "delta")', 'all words are there';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $k = $h.words.kv;
    is $h.tell, 23, '.words.kv advances the handle immediately';
    is $k.List.raku, '(0, "alpha", 1, "beta", 2, "gamma", 3, "delta")', 'indexed words';
    $h.close;
}
{
    my $h = $path.open(:r);
    is $h.words(:close).elems, 4, ':close still yields every word';
    nok $h.opened, 'and closes the handle';
}
{
    my $h = $path.open(:r);
    $h.get;
    is words($h).raku, '("gamma", "delta").Seq', 'the sub form reads from the current position';
    $h.close;
}
