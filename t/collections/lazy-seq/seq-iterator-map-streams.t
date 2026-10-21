use Test;

# `Seq.map`/`.grep` over a `Seq.new($iterator)` stream the iterator: only the
# elements a consumer pulls are produced, whether or not the iterator says
# `is-lazy` (#14271). The `start`/`await` guard keeps a regression from hanging
# the suite.

plan 12;

class Counting does Iterator {
    has $.i = 0;
    method pull-one { ++$!i }
}
class Finite does Iterator {
    has $.i = 0;
    method pull-one { $!i < 5 ?? ++$!i !! IterationEnd }
}
class LazyCounting does Iterator {
    has $.i = 0;
    method pull-one { ++$!i }
    method is-lazy { True }
}

sub guarded(&code) {
    my $p = start code();
    my $t = Promise.in(10).then({ 'timeout' });
    await Promise.anyof($p, $t);
    $p.status eq 'Kept' ?? $p.result !! 'timeout';
}

is-deeply guarded({ Seq.new(Counting.new).map(* + 1).head(3).List }), (2, 3, 4), "map over an unbounded iterator";
is-deeply guarded({ Seq.new(Counting.new).grep(* %% 2).head(3).List }), (2, 4, 6), "grep over an unbounded iterator";
is-deeply guarded({ Seq.new(Counting.new).map(* + 1).map(* * 2).head(3).List }), (4, 6, 8), "chained maps";
is-deeply guarded({ Seq.new(LazyCounting.new).map(* + 1).head(3).List }), (2, 3, 4), "an iterator that says it is lazy";
is Seq.new(Counting.new).map(* + 1).is-lazy, False, "the stage is lazy only when its source is";
is Seq.new(LazyCounting.new).map(* + 1).is-lazy, True, "...and is when it says so";

is Seq.new(Finite.new).map(* + 1).elems, 5, "a finite iterator still counts";
is-deeply Seq.new(Finite.new).map(* + 1).list, (2, 3, 4, 5, 6).Seq.list, "a finite map lists in full";
is-deeply Seq.new(Finite.new).grep(* %% 2).Array, [2, 4], "a finite grep";
is Seq.new(Finite.new).map(* + 1).sum, 20, "sum of a finite map";
my @a = Seq.new(Finite.new).map(* + 1);
is @a.join(","), "2,3,4,5,6", "assigned to an array";

my @seen;
my $s = Seq.new(Finite.new).map({ @seen.push($_); $_ });
my $before = @seen.elems;
my @h = $s.head(2);
is "$before {@seen.elems}", "0 2", "callbacks run on demand";
