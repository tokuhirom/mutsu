use v6;
use Test;

# Combinators over a *live* (Supplier-backed) Supply must compose: each stage
# builds a derived supplier, and a value emitted at the head has to be driven
# through EVERY stage, not just the first.
#
# Each emission produces a list of actions on the emitting supplier, and every
# re-emit site used to run only the `Call` (plain tap callback) action on the
# next supplier, dropping the twelve other action kinds. A derived supplier
# whose own tap was itself a combinator therefore received nothing: the value
# was computed and then discarded one hop upstream.
#
# `Supply.produce` had a second, independent version of the same flaw: instead
# of owning a derived supplier it kept the SOURCE's supplier id and stashed a
# `produce_callable` attribute to be consumed at tap time, so the next
# combinator registered on the shared id and silently dropped the produce.
#
# From SupplyTimeWindow 0.0.1's t/02-use.t (issue #7995), whose
# `.map(...).produce(...).map(...)` pipeline never emitted anything at all and
# left the test hanging forever on `await`.
#
# `head`/`unique`/`lines`/`words`/`elems` (issue #8474) had the same defect
# `produce` did: each handed back a Supply carrying the SOURCE's supplier id
# plus a marker attribute (`head_limit`, `unique_filter`, `is_lines`,
# `is_words`, `elems_filter`) consumed only at tap time, so chaining another
# combinator onto one of them read the marker-less shared id and dropped the
# stage, and chaining one of them onto another combinator worked only because
# it happened to be last. Fixed the same way `produce` was: each now owns a
# real derived supplier of its own, fed by its own transform tap on the
# source.

plan 31;

# map -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* + 1).map(* * 10).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 3;
    is-deeply @got, [20, 30, 40], 'map chained onto map sees every value';
}

# map -> grep
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* * 2).grep(* > 4).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 4;
    is-deeply @got, [6, 8], 'grep chained onto map filters the mapped values';
}

# grep -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.grep(* %% 2).map(* ~ '!').tap({ @got.push($_) });
    $s.emit($_) for 1 .. 5;
    is-deeply @got, ['2!', '4!'], 'map chained onto grep sees the kept values';
}

# map -> produce: the produce stage is reached at all ...
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* * 10).produce(-> $a, $b { $a + $b }).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 3;
    is-deeply @got, [10, 30, 60], 'produce chained onto map folds the mapped values';
}

# ... and produce -> map: the produce stage is not skipped by the next one.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.produce(-> $a, $b { $a + $b }).map(* * 10).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 3;
    is-deeply @got, [10, 30, 60], 'map chained onto produce maps the running totals';
}

# The SupplyTimeWindow shape: map -> produce -> map, three live stages.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply
      .map(-> $i { [$i] })
      .produce(-> @a, @b { [|@a, |@b] })
      .map(-> @w { @w.join('-') })
      .tap({ @got.push($_) });
    $s.emit($_) for 1 .. 3;
    is-deeply @got, ['1', '1-2', '1-2-3'], 'a three-stage live pipeline delivers every stage';
}

# `done` must reach the end of the chain too, not just the first derived
# supply: an `await` on a Promise kept from the last stage's done callback
# hung forever when the propagation stopped after one hop.
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.map(* + 1).map(* * 2).tap(-> $ { }, done => { $done++ });
    $s.emit(1);
    $s.done;
    is $done, 1, 'done propagates through every stage of a map chain';
}

{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.map(* + 1).produce(-> $a, $b { $a + $b }).tap(-> $ { }, done => { $done++ });
    $s.emit(1);
    $s.done;
    is $done, 1, 'done propagates through a produce stage';
}

# A `react whenever` over a chained live Supply is the shape the distribution
# actually used; it must see the values and the completion.
{
    my $s = Supplier.new;
    my @got;
    my $fin = Promise.new;
    start {
        react whenever $s.Supply.map(* + 1).produce(-> $a, $b { $a + $b }) -> $v {
            @got.push($v);
            done if @got.elems == 3;
        }
        $fin.keep;
    }
    sleep 0.5;
    $s.emit($_) for 1 .. 3;
    await Promise.anyof($fin, Promise.in(10));
    is $fin.status, Kept, 'react over a chained live Supply completes';
    is-deeply @got, [2, 5, 9], 'react over a chained live Supply sees every value';
}

# `.live` is per-combinator in rakudo and must not follow from the fact that a
# stage now carries a supplier id of its own: map/grep stay live over a live
# source, produce does not.
{
    my $s = Supplier.new;
    ok $s.Supply.map(*.self).live, 'map over a live source is still live';
    nok $s.Supply.produce(-> $a, $b { $a }).live, 'produce over a live source is not live';
}

# The single-stage cases the chain fix must not disturb.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.produce(-> $a, $b { $a ~ $b }).tap({ @got.push($_) });
    $s.emit($_) for <a b c>;
    is-deeply @got, ['a', 'ab', 'abc'], 'a bare produce still emits every running value';
}

{
    my $s = Supplier.new;
    my @got;
    my $reduced;
    $s.Supply.reduce(-> $a, $b { $a + $b }).tap({ $reduced = $_ });
    $s.emit($_) for 1 .. 4;
    $s.done;
    is $reduced, 10, 'reduce still emits its single folded value at done';
}

# head -> map: the head stage is reached at all ...
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.head(2).map(* * 10).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 4;
    is-deeply @got, [10, 20], 'map chained onto head sees only the limited values';
}

# ... and map -> head: head is not skipped by the previous stage.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* * 10).head(2).tap({ @got.push($_) });
    $s.emit($_) for 1 .. 4;
    is-deeply @got, [10, 20], 'head chained onto map still limits the mapped values';
}

# unique -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.unique.map(* ~ '!').tap({ @got.push($_) });
    $s.emit($_) for 1, 1, 2, 3, 2;
    is-deeply @got, ['1!', '2!', '3!'], 'map chained onto unique sees only the unique values';
}

# map -> unique
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* % 3).unique.tap({ @got.push($_) });
    $s.emit($_) for 1, 4, 2, 7, 5;
    is-deeply @got, [1, 2], 'unique chained onto map filters the mapped values';
}

# lines -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.lines.map(*.uc).tap({ @got.push($_) });
    $s.emit("ab\ncd\n");
    is-deeply @got, ['AB', 'CD'], 'map chained onto lines sees the split lines';
}

# words -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.words.map(*.uc).tap({ @got.push($_) });
    $s.emit("ab cd ef\n");
    is-deeply @got, ['AB', 'CD', 'EF'], 'map chained onto words sees the split words';
}

# elems -> map
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.elems.map(* * 100).tap({ @got.push($_) });
    $s.emit($_) for 'a', 'b', 'c';
    is-deeply @got, [100, 200, 300], 'map chained onto elems sees the running counts';
}

# `done` must reach the end of the chain for each of these five too.
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.head(5).map(* + 1).tap(-> $ { }, done => { $done++ });
    $s.emit(1);
    $s.done;
    is $done, 1, 'done propagates through a head stage whose limit was not reached';
}
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.unique.map(* + 1).tap(-> $ { }, done => { $done++ });
    $s.emit(1);
    $s.done;
    is $done, 1, 'done propagates through a unique stage';
}
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.lines.map(*.uc).tap(-> $ { }, done => { $done++ });
    $s.emit("no newline");
    $s.done;
    is $done, 1, 'done propagates through a lines stage, flushing the trailing partial line';
}
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.words.map(*.uc).tap(-> $ { }, done => { $done++ });
    $s.emit("no trailing ws");
    $s.done;
    is $done, 1, 'done propagates through a words stage, flushing the trailing partial word';
}
{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.elems.map(* + 1).tap(-> $ { }, done => { $done++ });
    $s.emit(1);
    $s.done;
    is $done, 1, 'done propagates through an elems stage';
}

# Reaching a head limit finishes head's OWN derived supplier -- other stages
# chained off the same source must still see later values.
{
    my $s = Supplier.new;
    my @head_got;
    my @map_got;
    $s.Supply.head(2).tap({ @head_got.push($_) });
    $s.Supply.map(* * 10).tap({ @map_got.push($_) });
    $s.emit($_) for 1, 2, 3, 4;
    is-deeply @head_got, [1, 2], 'head stops at its own limit';
    is-deeply @map_got, [10, 20, 30, 40],
        'a sibling stage on the same source keeps seeing every value';
}

# `.live` is False for all five (like `produce`/`batch`), unlike `map`/`grep`.
{
    my $s = Supplier.new;
    nok $s.Supply.head(2).live, 'head over a live source is not live';
    nok $s.Supply.unique.live, 'unique over a live source is not live';
    nok $s.Supply.lines.live, 'lines over a live source is not live';
}
