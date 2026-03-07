use Test;
use lib $*PROGRAM.parent.add("roast/packages/Test-Helpers");
use Test::Tap;

plan 5;

dies-ok { Supply.reduce({ ... }) }, "Supply.reduce dies on type object";
dies-ok { Supplier.new.Supply.reduce(23) }, "Supply.reduce requires code when specified";

tap-ok Supply.from-list(1..5).reduce({ $^a + $^b }), [15], "Supply.reduce sums values";
tap-ok Supply.from-list("a".."e").reduce(&infix:<~>), [<abcde>], "Supply.reduce concatenates";

{
    my $s = Supplier.new;
    tap-ok $s.Supply.reduce(&infix:<(+)>),
      [
        (a => 13, b => 4, c => 42, e => 10).Bag,
      ],
      "Supply.reduce combines supplier stream",
      :after-tap({
          $s.emit({ a => 1, b => 2 });
          $s.emit({ c => 42, b => 2 });
          $s.emit({ a => 12, e => 10 });
          $s.done;
      });
}
