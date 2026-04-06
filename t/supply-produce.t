use Test;
use lib $*PROGRAM.parent.add("roast/packages/Test-Helpers");
use Test::Tap;

plan 4;

dies-ok { Supply.produce({ ... }) }, "Supply.produce dies on type object";
dies-ok { Supplier.new.Supply.produce(23) }, "Supply.produce requires code when specified";

tap-ok Supply.from-list(1..5).produce({ $^a + $^b }), [1, 3, 6, 10, 15],
  "Supply.produce scans numeric values";

{
    my $s = Supplier.new;
    tap-ok $s.Supply.produce(&infix:<(+)>),
      [
        { a => 1, b => 2 },
        (a => 1, b => 4, c => 42).Bag,
        (a => 13, b => 4, c => 42, e => 10).Bag,
      ],
      "Supply.produce combines supplier stream",
      :after-tap({
          $s.emit({ a => 1, b => 2 });
          $s.emit({ c => 42, b => 2 });
          $s.emit({ a => 12, e => 10 });
          $s.done;
      });
}
