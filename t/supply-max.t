use Test;

plan 6;

dies-ok { Supply.max }, "Supply.max dies as class method";
dies-ok { Supply.new.max(23) }, "Supply.max requires code when specified";

my @asc;
Supply.from-list(1..5).max.tap(-> $v { @asc.push($v) });
is-deeply @asc, [1, 2, 3, 4, 5], "Supply.max emits running maxima";

my @desc;
Supply.from-list(5...1).max.tap(-> $v { @desc.push($v) });
is-deeply @desc, [5], "Supply.max keeps only first peak for descending input";

my @by-key;
Supply.from-list("a", "B", "c", "D").max(-> $_ { $_.uc }).tap(-> $v { @by-key.push($v) });
is-deeply @by-key, ["a", "B", "c", "D"], "Supply.max with key extractor works";

my @by-cmp;
Supply.from-list(1, 3, 2, 5).max(-> $a, $b { $a <=> $b }).tap(-> $v { @by-cmp.push($v) });
is-deeply @by-cmp, [1, 3, 5], "Supply.max with comparator block works";
