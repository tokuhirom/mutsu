use Test;

plan 6;

dies-ok { Supply.minmax }, "Supply.minmax dies as class method";
dies-ok { Supply.new.minmax(23) }, "Supply.minmax mapper must be callable";

my @ascending;
Supply.from-list(1..5).minmax.tap(-> $v { @ascending.push($v) });
is-deeply @ascending, [(1..1), (1..2), (1..3), (1..4), (1..5)],
  "Supply.minmax emits running ranges for ascending values";

my @descending;
Supply.from-list(5...1).minmax.tap(-> $v { @descending.push($v) });
is-deeply @descending, [(5..5), (4..5), (3..5), (2..5), (1..5)],
  "Supply.minmax emits running ranges for descending values";

my @ascending_alpha;
Supply.from-list(flat("a".."e", "A".."E")).minmax(*.uc).tap(
  -> $v { @ascending_alpha.push($v) }
);
is-deeply @ascending_alpha, [("a".."a"), ("a".."b"), ("a".."c"), ("a".."d"), ("a".."e")],
  "Supply.minmax mapper tracks ascending alpha ranges";

my @descending_alpha;
Supply.from-list(reverse(flat "a".."e", "A".."E")).minmax(*.lc).tap(
  -> $v { @descending_alpha.push($v) }
);
is-deeply @descending_alpha, [("E".."E"), ("D".."E"), ("C".."E"), ("B".."E"), ("A".."E")],
  "Supply.minmax mapper tracks descending alpha ranges";
