use v6;
use Test;

# `IO::Handle.lines` is a lazy Seq, and so are the stages chained on it:
# nothing is read from the handle until the stage is consumed (#16065).

plan 33;

my $path = $*TMPDIR.add("lines-stage-lazy-{$*PID}-{(^1000000).pick}.txt");
$path.spurt("alpha beta\ngamma delta\nepsilon\n");
LEAVE try $path.unlink;

# Building a stage reads nothing.
for <pairs antipairs> -> $m {
    my $h = $path.open(:r);
    my $s = $h.lines."$m"();
    is $h.tell, 0, ".lines.$m leaves the handle unread";
    $h.close;
}
{
    my $h = $path.open(:r);
    my $s = $h.lines.map(*.uc);
    is $h.tell, 0, '.lines.map leaves the handle unread';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $s = $h.lines.grep(*.chars > 5);
    is $h.tell, 0, '.lines.grep leaves the handle unread';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $s = $h.lines.rotor(2);
    is $h.tell, 0, '.lines.rotor leaves the handle unread';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $s = $h.lines.map(*.uc).grep(*.chars > 5).map(*.lc);
    is $h.tell, 0, 'a chain of stages leaves the handle unread';
    $h.close;
}

# Full consumption gives the same values.
{
    my $h = $path.open(:r);
    is-deeply $h.lines.map(*.uc).List, ("ALPHA BETA", "GAMMA DELTA", "EPSILON"), 'map';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.grep(*.contains('a')).List, ("alpha beta", "gamma delta"), 'grep';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.pairs.List, (0 => "alpha beta", 1 => "gamma delta", 2 => "epsilon"), 'pairs';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.antipairs.List, ("alpha beta" => 0, "gamma delta" => 1, "epsilon" => 2), 'antipairs';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.rotor(2).List, (("alpha beta", "gamma delta"),), 'rotor drops the partial tail';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.rotor(2, :partial).List, (("alpha beta", "gamma delta"), ("epsilon",)), 'rotor :partial';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.map(*.uc).grep(*.chars > 7).map(*.lc).List,
        ("alpha beta", "gamma delta"), 'map.grep.map';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.words.map(*.chars).List, (5, 4, 5, 5, 7), 'words.map';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.pairs.map({ .key + .value.chars }).List, (10, 12, 9), 'pairs.map';
    $h.close;
}

# A stage reads one line per element it hands out.
{
    my $h = $path.open(:r);
    my $first = $h.lines.map(*.uc).head(1).List;
    is-deeply $first, ("ALPHA BETA",), 'head(1) of a map';
    is $h.tell, "alpha beta\n".chars, 'only the first line was read';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $seen = 0;
    my $first = $h.lines.map({ $seen++; .uc }).head(2).eager;
    is $seen, 2, 'the map callback ran for the two lines pulled';
    $h.close;
}
{
    my $h = $path.open(:r);
    for $h.lines.map(*.uc) -> $l {
        is $l, "ALPHA BETA", 'for over a map streams';
        is $h.tell, "alpha beta\n".chars, 'one line read per iteration';
        last;
    }
    $h.close;
}

# A stage consumes its receiver.
{
    my $h = $path.open(:r);
    my $s = $h.lines;
    my $m = $s.map(*.uc);
    throws-like { $s.List }, X::Seq::Consumed, 'the receiver of map is consumed';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $s = $h.lines;
    my $p = $s.pairs;
    throws-like { $s.List }, X::Seq::Consumed, 'the receiver of pairs is consumed';
    $h.close;
}

# The stage is a Seq that can be consumed once.
{
    my $h = $path.open(:r);
    my $m = $h.lines.map(*.uc);
    is $m.elems, 3, 'elems of a map over lines';
    $h.close;
}
{
    my $h = $path.open(:r);
    my $m = $h.lines.map(*.uc);
    is $m.join(','), 'ALPHA BETA,GAMMA DELTA,EPSILON', 'join of a map over lines';
    $h.close;
}

# `:close` closes the handle once the stage is exhausted.
{
    my $h = $path.open(:r);
    my @l = $h.words(:close).map(*.uc);
    is @l.elems, 5, 'words(:close).map';
    nok $h.opened, 'the handle was closed on exhaustion';
}

# `IO::Path.lines` and `lines($fh)` take the same stages.
is-deeply $path.lines.map(*.chars).List, (10, 11, 7), 'IO::Path.lines.map';
is-deeply $path.lines.pairs.map(*.key).List, (0, 1, 2), 'IO::Path.lines.pairs';
{
    my $h = $path.open(:r);
    is-deeply lines($h).grep(*.contains('e')).List, ("alpha beta", "gamma delta", "epsilon"), 'lines($fh).grep';
    $h.close;
}

# Words and kv keep working.
{
    my $h = $path.open(:r);
    is-deeply $h.words.pairs.map(*.key).List, (0, 1, 2, 3, 4), 'words.pairs';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.kv.List, (0, "alpha beta", 1, "gamma delta", 2, "epsilon"), 'kv';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.batch(2).List, (("alpha beta", "gamma delta"), ("epsilon",)), 'batch';
    $h.close;
}
{
    my $h = $path.open(:r);
    is-deeply $h.lines.skip(1).List, ("gamma delta", "epsilon"), 'skip';
    $h.close;
}
