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

plan 12;

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
