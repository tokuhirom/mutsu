use Test;

# #9255: `.comb` with no positional matcher on an IO::Handle combs the
# handle's text into characters, like `Str.comb` -- it used to return `().Seq`.

plan 6;

my $path = $*TMPDIR.add("mutsu-handle-comb-{$*PID}.txt");
$path.spurt("ab\ncd\n");
LEAVE $path.unlink;

is-deeply $path.open.comb, ("a", "b", "\n", "c", "d", "\n").Seq, 'handle .comb with no args';
is-deeply $path.open.comb(/\w/), ("a", "b", "c", "d").Seq, 'handle .comb with a regex';

{
    my $fh = $path.open;
    is-deeply $fh.comb(:close), ("a", "b", "\n", "c", "d", "\n").Seq, 'handle .comb(:close) with no matcher';
    nok $fh.opened, ':close closed the handle';
}

is-deeply $path.comb, ("a", "b", "\n", "c", "d", "\n").Seq, 'IO::Path .comb with no args';
is-deeply "ab".comb(:match), ("a", "b").Seq, 'Str.comb with only a named arg';
