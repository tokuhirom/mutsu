# An array alias survives a thread having run

Once any thread had run, a plain-lexical `@a.push` in the *main* thread was
routed through the cross-thread `__mutsu_atomic_arr::` store — unconditionally,
because `shared_vars_active` is set at the first spawn and never goes back to
false. That store is keyed by NAME, so the push detached the array from every
other binding of the same container:

```raku
sub aliased() {
    my @a; my @b; my $cond = False;
    my @t := $cond ?? @b !! @a;   # @t and @a are the same container
    @a.push("via-a");
    @t.push("via-t");
    @a.join(",")
}
say aliased();          # via-a,via-t
await start { 42 };
say aliased();          # was: via-t   (the @a push landed under a different key)
```

`@t =:= @a` still reported `True` — the bind itself was fine. The `@a` push
went to `__mutsu_atomic_arr::@a` while `@t` kept holding the original `Gc`
node, so each binding saw only its own writes. Any program that spawns a thread
once — every Cro application — ran the rest of its life this way.

## The fix

`exec_array_push_op` now takes the name-keyed store only when the frame can
actually be racing: it is a worker thread (where serializing concurrent appends
is the whole point), or the name is *genuinely shared* — it already has an
authoritative `__mutsu_atomic_arr::` entry, or the base name is bound in the
cross-thread store because a spawn happened while that lexical was live. This
is the gate `assign_array_elem_to_shared_var` already applied to element
assignment; the push path is now consistent with it.

The worker-thread arm is deliberately kept unconditional. `append`/`prepend`/
`unshift`/`splice` seed and use the atomic entry there, and reads prefer it once
it exists, so a `push` that skipped the store would be silently lost by the next
`append` — the zef `populate-distributions` bug pinned by
`t/hyper-array-mutators.t`.

Pinned by `t/array-alias-after-thread.t`, which also re-checks that concurrent
`start { @shared.push($i) }` still merges without losing an element.

## Why it mattered

`Cro::HTTP::Router` compiles each route's segment matcher with
`my @matcher-target := $param.optional ?? @segments-optional !! @segments-required;`
followed by `@matcher-target.push(...)`. Once a route block had actually served
a request (which runs the handler in a `start`), every subsequent route block
lost every segment it pushed, so `get -> 'product' { … }` compiled to a matcher
with no path segment. With this and the single-literal-parameter fix,
`t/http-router.rakutest` goes from 31 of 51 passing subtests to 49.
