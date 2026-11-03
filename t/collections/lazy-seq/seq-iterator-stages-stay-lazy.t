use Test;
plan 11;

# `.pairs` / `.antipairs` / `.rotor` over `Seq.new($iterator)` are lazy stages:
# the call pulls nothing, the consumer does (#16132).
my @log;
class It does Iterator {
    has $.n is rw = 0;
    method pull-one { @log.push("pull"); $!n++ < 3 ?? $!n !! IterationEnd }
}

for <pairs antipairs> -> $m {
    @log = ();
    my $s = Seq.new(It.new)."$m"();
    is @log.elems, 0, ".$m pulls nothing when called";
    my @r = $s.list;
    is @log.elems, 4, ".$m pulls when consumed";
}
is-deeply Seq.new(It.new).pairs.list.Array, [0 => 1, 1 => 2, 2 => 3], 'pairs values';
is-deeply Seq.new(It.new).antipairs.list.Array, [1 => 0, 2 => 1, 3 => 2], 'antipairs values';

@log = ();
my $r = Seq.new(It.new).rotor(2);
is @log.elems, 0, '.rotor pulls nothing when called';
is $r.list.raku, ((1, 2),).raku, 'rotor values';

class Endless does Iterator {
    has $.n is rw = 0;
    method pull-one { @log.push("pull"); $!n++ }
}
@log = ();
is-deeply Seq.new(Endless.new).pairs.head(2).list.Array, [0 => 0, 1 => 1], 'endless iterator: pairs.head(2)';
is @log.elems, 2, 'only the requested prefix was pulled';
is-deeply Seq.new(Endless.new).rotor(2).head(2).list.Array, [(0, 1), (2, 3)], 'endless iterator: rotor.head';
