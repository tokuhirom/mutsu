use Test;
use lib $*PROGRAM.parent.add("roast/packages/Test-Helpers");
use Test::Tap;

plan 2;

{
    my $s = Supplier.new;
    tap-ok $s.Supply.unique(:expires(2)),
      [1, 2, 3, 1, 2],
      "tap-ok honors Supply.unique(:expires) for supplier-backed supplies",
      :after-tap({
          $s.emit(1);
          sleep 1;
          $s.emit(2);
          sleep 1;
          $s.emit(3);
          sleep 1;
          $s.emit(1);
          $s.emit(2);
          $s.emit(3); # still within expiry window of the previous 3
          $s.done;
      });
}

{
    my $s = Supplier.new;
    tap-ok $s.Supply.unique(
        :as(*.substr(0, 1)),
        :with({ $^a.lc eq $^b.lc }),
        :expires(2),
    ),
      [<a bb c B>],
      "tap-ok applies :as/:with/:expires together on supplier-backed unique",
      :after-tap({
          $s.emit("a");
          sleep 1;
          $s.emit("bb");
          sleep 1;
          $s.emit("B");  # same key as "bb"
          sleep 1;
          $s.emit("c");
          $s.emit("B");
          $s.emit("bb"); # same key as "B"
          $s.done;
      });
}
