use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Tap;

# NOTE: the Supplier is `$src`, not `$s`, on purpose. `Test::Tap::tap-ok`'s own
# first parameter is `$s`, and a caller closure whose captured lexical collides
# with a callee parameter is shadowed when the closure is invoked from a nested
# block that reads that parameter -- see
# todo/deep/closure-capture-shadowed-by-colliding-callee-parameter.md.

plan 2;

{
    my $src = Supplier.new;
    tap-ok $src.Supply.unique(:expires(2)),
      [1, 2, 3, 1, 2],
      "tap-ok honors Supply.unique(:expires) for supplier-backed supplies",
      :after-tap({
          $src.emit(1);
          sleep 1.5;
          $src.emit(2);
          sleep 1.5;
          $src.emit(3);
          sleep 1.5;
          $src.emit(1);
          $src.emit(2);
          $src.emit(3); # still within expiry window of the previous 3
          $src.done;
      });
}

{
    my $src = Supplier.new;
    tap-ok $src.Supply.unique(
        :as(*.substr(0, 1)),
        :with({ $^a.lc eq $^b.lc }),
        :expires(2),
    ),
      [<a bb c B>],
      "tap-ok applies :as/:with/:expires together on supplier-backed unique",
      :after-tap({
          $src.emit("a");
          sleep 1.5;
          $src.emit("bb");
          sleep 1.5;
          $src.emit("B");  # same key as "bb"
          sleep 1.5;
          $src.emit("c");
          $src.emit("B");
          $src.emit("bb"); # same key as "B"
          $src.done;
      });
}
