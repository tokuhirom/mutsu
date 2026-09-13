# An associative subscript on a `Pair` reaches its value, and stores into it when it is a container

`Pair` DOES `Associative`, so `$p<key>` descends into it — and `Pair.AT-KEY`
hands back `.value` **itself**, because a `Pair` gives its value no `Scalar` of
its own. The subscript is therefore not a location, and the store written
through it is the reached value's own business:

```raku
my $p = (c => [1, 2]);  $p<c> = [3, 4];   # :c([3, 4])   -- Array.STORE
my $p = (c => [1, 2]);  $p<c> = 5;        # :c([5])      -- ditto, one element
my $p = (c => {:a(1)}); $p<c> = {:b(2)};  # :c({:b(2)})  -- Hash.STORE
my $p = (c => 1);       $p<c> = 9;        # Cannot modify an immutable Int (1)
```

[#8275](https://github.com/tokuhirom/mutsu/pull/8275) taught mutsu the refusing
half of that rule and applied it to every value alike, so the three rows above
that rakudo stores died as well. The three *descending* spellings
(`$p<c>[0] = 9`, `$p<c><a> = 7`, `%h<x><y>[0] = 9`) were worse: they reached no
arm that recognizes a `Pair` as a container, wrote nowhere, and reported
success. A 3+-level chain rebuilt the `Pair` chain as nested `Hash`es instead
(`%h<x><y><z> = [3,4]` answered `{:x(${:y({:z($[3, 4])})})}` where rakudo keeps
`{:x(${:y(:z([3, 4]))})}`).

The rule now lives in one predicate, `pair_subscript_aggregate`, whose `Some`
is exactly `pair_subscript_store_refusal`'s `None`: an `Array`/`Hash` behind the
addressed key is a real container and takes the write, everything else —
including an immutable `List`, a `Map` and a `Seq` — is refused at the value it
reaches. Each store site then either STOREs into that container
(`store_into_pair_aggregate`, mutating the shared node in place so the `Pair`
and every other alias of the container see it) or descends into it: the
single-subscript named store, both two-level arms, the 3+-level walk's
intermediate and final levels, the computed-target store, and the path-accessor
store. Two supporting bugs came out with it — `subscript_descent_refusal_at`
answered "Type Pair does not support associative indexing" for a `Pair` that IS
descendable, and the deep walk passed the *next* level's positional flag where
it needed the flag of the subscript actually being applied.

The whole-container STORE takes the rvalue *un-itemized*: ADR-0040's
element-store itemization hook fires above every one of these ops, and an
itemized `[3, 4]` would have STOREd as `[[3, 4],]`. A genuinely itemized rvalue
(`my $x = [3, 4]; $p<c> = $x`) still nests, exactly as `@a = $x` does.

Found re-measuring [#7539](https://github.com/tokuhirom/mutsu/issues/7539)'s
`Config::TOML` + `Crane` battery pair, where it was the last failure in
`Crane`'s `add.rakutest`: a nested in-place add
(`Crane.add(%i, :path<a b c>, :value(...), :in-place)` over the colonpair
fixture `:a({:b(:c([...]))})`) reaches
`Crane::At.at($root, @path){$step} = $value`, whose `$step` lands on the `Pair`
`c => [...]`. `Crane` goes from 9/15 files to 10/15; `Config::TOML` is unchanged
at 14/19, with no regression.

Pinned by `t/collections/range-pair/pair-subscript-stores-into-a-container-value.t`
(23 assertions), verified to pass under rakudo v2026.07 as well as under mutsu.
