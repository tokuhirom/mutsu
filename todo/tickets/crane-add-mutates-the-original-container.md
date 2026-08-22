# `Crane.add` mutates the original container and returns the unmodified copy

## Symptom

`Crane`'s non-destructive operations (`add`, `copy`, `move`, `remove`,
`replace`) each take a `deepmap({ .clone })` snapshot of the caller's container,
mutate the *snapshot*, and return it. Under mutsu the two are swapped: the
caller's original is mutated and the returned value is the unmodified copy.

```raku
use Crane;                                  # -I <Crane-0.1.2>/lib
my %h = a => { b => 1 };
my %r = Crane.add(%h, :path["a", "c"], :value(2));
say "orig:   ", %h.raku;
say "result: ", %r.raku;

# raku:   orig: {:a(${:b(1)})}            result: {:a(${:b(1), :c(2)})}
# mutsu:  orig: {:a(${:b(1), :c(2)})}     result: {:a(${:b(1)})}
```

This is what the "Original container is unchanged" assertions in Crane's
`t/add.rakutest`, `t/copy.rakutest`, `t/move.rakutest`, `t/remove.rakutest` and
`t/replace.rakutest` are failing on — collectively the largest remaining block
of Crane subtest failures (roughly 120 of ~171), and therefore a gate on the
`Config::TOML` battery (`docs/batteries/toml.md`).

## What is NOT the cause

`deepmap({ .clone })` itself is correct. In isolation mutsu matches raku
byte-for-byte, for both nested hashes and nested arrays:

```raku
my %h = a => { b => 1 };
my $root = %h.deepmap({ .clone });
$root<a><c> = 2;
say %h.raku;      # {:a(${:b(1)})}      — original untouched, same as raku
say $root.raku;   # ${:a(${:b(1), :c(2)})}
```

So the snapshot is a genuine deep copy. The divergence appears once the write
goes through Crane's own lvalue plumbing rather than a literal subscript.

## Where to look

`Crane::Add`'s bodies have the shape (`lib/Crane/Add.rakumod`):

```raku
my $root = container.deepmap({ .clone });
Crane::At.at($root, @path){$step} = $value.clone;
return $root;
```

`container` is a sigilless `\`-parameter bound to the caller's hash, and
`Crane::At.at` is an `is rw` routine returning a container (ADR-0059). The
suspicion is that the lvalue container `at($root, …)` hands back is anchored to
`container` — the caller's original — rather than to the freshly-deepmapped
`$root`, i.e. the sigilless binding or the `is rw` descent is resolving the
*parameter* instead of the local. That would explain both halves of the swap in
one mechanism.

Next step is to bisect that: reduce `Crane::At.at` to a standalone `is rw`
routine called on a deepmapped local and see whether the write lands in the
local or in the routine's caller-side source.

## Provenance

Found while measuring `Crane` after
`news/2026-08/deferred-vivification-path-steps-are-typed.md` closed the
positional-step gap. Independent of that change: it reproduces identically
before and after it.
