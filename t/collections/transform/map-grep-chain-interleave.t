use Test;

# A `.grep(...).map(...)` chain is a pull pipeline: each element passes
# through the grep callback and then immediately through the map callback,
# so the stages interleave per element (#11176).

plan 11;

{
    my @l = "a #| x", "b", "c #| y";
    my @d = @l.grep({ m/ '#|' \s* (.*) $/ }).map({ ~$0 });
    is-deeply @d, ["x", "y"], 'map reads the $/ the grep set for the same element';
}

{
    my @log;
    my @r = (1..4).grep({ @log.push("g$_"); $_ %% 2 }).map({ @log.push("m$_"); $_ * 10 });
    is-deeply @r, [20, 40], 'grep.map result';
    is-deeply @log, [<g1 g2 m2 g3 g4 m4>], 'grep and map callbacks interleave';
}

{
    my @log;
    my @r = (1..4).map({ @log.push("m$_"); $_ + 1 }).grep({ @log.push("g$_"); $_ %% 2 });
    is-deeply @r, [2, 4], 'map.grep result';
    is-deeply @log, [<m1 g2 m2 g3 m3 g4 m4 g5>], 'map and grep callbacks interleave';
}

{
    my @log;
    my @r = (1..6).grep({ @log.push("g$_"); True }).map({ @log.push("m$_"); last if $_ == 3; $_ });
    is-deeply @r, [1, 2], '`last` in the downstream map stops the chain';
    is-deeply @log, [<g1 m1 g2 m2 g3 m3>], 'the upstream grep stops with it';
}

is-deeply (1..10).grep(* %% 2).map(* * 3).grep(* > 10).head(2).List, (12, 18),
    'a three-stage chain with .head';

is-deeply (1..7).grep(* > 1).map(-> $a, $b { $a + $b }).List, (5, 9, 13),
    'a two-parameter map over a grep';

{
    my @log;
    for (1..3).grep({ @log.push("G$_"); True }).map({ @log.push("M$_"); $_ }) {
        @log.push("B$_");
    }
    is-deeply @log, [<G1 M1 B1 G2 M2 B2 G3 M3 B3>], 'a for loop over the chain interleaves all three';
}

{
    my $g = (1..3).grep(*.so);
    $g.map(* + 1).eager;
    throws-like { $g.map(* + 1).eager }, X::Seq::Consumed,
        'chaining onto a grep Seq consumes it';
}
