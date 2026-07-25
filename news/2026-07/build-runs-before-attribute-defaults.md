# `submethod BUILD` now runs before the attribute initializers, not after

Raku's construction order is: run the user's `submethod BUILD`, *then* apply each
`has $.x = <default>` initializer, and only to the attributes BUILD did not set.
mutsu did the opposite — it evaluated every initializer first and then ran BUILD
— which made two things observably wrong:

1. an initializer that reads a sibling attribute read the sibling's *pre-BUILD*
   value instead of the value BUILD gave it;
2. an initializer with a side effect ran even when BUILD set the attribute.

```raku
class D {
    has $.x = 100;
    has $!y = $!x;
    submethod BUILD(:$!x = 200) { }
    method show { say "$!x $!y" }
}
D.new.show;    # raku: 200 200      mutsu (before): 200 100
```

## What changed

Construction now does a **seed / defer / apply** pass, in
`src/runtime/attr_build_defaults.rs`. A class with a BUILD phase seeds every
not-provided attribute that has an initializer with the same value an
initializer-less attribute would get — its nominal type object, native zero, or
empty container — records the initializer as deferred, runs BUILD against the
real instance, and only then applies the initializers BUILD left alone. That
also means BUILD now observes the same pre-initializer state rakudo shows it.

Rakudo marks "BUILD set this" with a null attribute slot, which has no
equivalent here, so mutsu uses two conditions:

- **an assignment reached the instance's shared cell.** `write_attr_cell_by_key`
  — the single choke point every attribute write funnels through, including the
  `mirror_attributive_params_to_cell` path — records into a frame pushed for the
  duration of the BUILD phase. This is what makes an explicit `$!x = Any`, and a
  `BUILD(:$!x)` attributive parameter that was never passed, suppress the
  initializer exactly as rakudo does.
- **the slot no longer holds its seed**, which catches an in-place container
  mutation. `has @.xs = 1, 2, 3` plus `BUILD { @!xs.push(9) }` yields `[9]` in
  rakudo (the vivification counts as setting it), and now in mutsu too.

All three construction paths move in lockstep: the full `.new` default
constructor and `bless` implement the deferral, and the native fast constructor
hands any class that has both a BUILD phase and an unprovided initializer over
to the interpreter path rather than filling defaults eagerly.

Two pieces of the `.new` path were lifted into the new module while they were
being reused by the post-BUILD pass — `seed_attr_value` (what an attribute with
no initializer starts life with) and `eval_attr_default_expr` (evaluating one
initializer with `self` and the sibling `$!a` / `$.a` bindings in place). The
pre-BUILD and post-BUILD passes differ only in what `self` is: a snapshot
instance before BUILD, the real instance after.

The pre-BUILD `where` / `:D` / `:U` checks skip a deferred attribute — its slot
still holds a seed at that point — and the existing post-BUILD run of the same
checks covers it instead.

## Impact

`Test::Scheduler` (`TODO_dist` T-037) was the reporting dist: its
`has $!virtual-target = $!virtual-time;` ended up a fraction of a second behind
`$!virtual-time`, so `advance-by($n)` computed a target *earlier* than the events
it should fire, `!run-due` classified every event as `future`, and the suite hung
on the first `await`. `t/not-time-based.rakutest` now passes 3/3 and the other
two files run instead of hanging (they now stop on an unrelated
`given $c -> &to-run` binding bug). Any dist using the very common
`has $!b = $!a;` + `submethod BUILD` pattern is affected the same way.

Pin: `t/build-attr-default-order.t` (15 assertions, all verified against raku).
