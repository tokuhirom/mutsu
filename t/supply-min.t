use Test;

plan 6;

dies-ok { Supply.min }, "Supply.min dies on type object";
dies-ok { Supply.new.min(23) }, "Supply.min mapper must be callable";

my @ascending;
Supply.from-list(1..10).min.tap(-> $v { @ascending.push($v) });
is-deeply @ascending, [1], "Supply.min keeps only new numeric minima";

my @descending;
Supply.from-list(10...1).min.tap(-> $v { @descending.push($v) });
is-deeply @descending, [10...1], "Supply.min emits each descending numeric minimum";

my @ascending_alpha;
Supply.from-list(("a".."e").Slip, ("A".."E").Slip).min(*.uc).tap(
    -> $v { @ascending_alpha.push($v) }
);
is-deeply @ascending_alpha, ["a"], "Supply.min mapper handles slipped ranges";

my @descending_alpha;
Supply.from-list("E"..."A", ("e".."a").Slip).min(*.lc).tap(
    -> $v { @descending_alpha.push($v) }
);
is-deeply @descending_alpha, [<E D C B A>], "Supply.min mapper tracks descending minima";
