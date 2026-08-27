# `BEGIN my (&plan, &is) = do { use Test; (&plan, &is) }` does not bind the imported routines

roast's idiom for importing only *some* of a module's exports is a BEGIN-time
list assignment of `&`-sigilled lexicals:

```raku
BEGIN my (&plan, &subtest, &is, &is-deeply, &throws-like) = do {
    use Test;
    (&plan, &subtest, &is, &is-deeply, &throws-like)
}

plan 55;
```

mutsu does not bind them, so the very next statement dies with
`Unknown function: plan`.

## Repro

`roast/S32-list/skip.t` (lines 1-8 are exactly the snippet above) under
`MUTSU_REAL_TEST=1`:

```
Unknown function: plan
  in block <unit> at roast/S32-list/skip.t line 8
```

The whole file — 55 tests — never runs.

## Why it only shows under the real `Test`

The idiom exists precisely so that `Test`'s own `skip` export does not shadow
the core list `skip` this file is testing (the file says so in its first two
lines). Under mutsu's native provider `use Test` installs its routines
globally regardless of the `do` block's scope, so `plan` resolves anyway and
the missing binding is invisible. The genuine upstream module is a real
lexical import, so the selective-import binding actually has to work.

Note the same collision has a `t/`-side counterpart,
`t/skip-list-vs-test.t`, which pins mutsu's native-provider behaviour
(core `skip` winning over `Test`'s) — a behaviour raku does **not** share:
with `Test` loaded, raku also routes `skip(2, <a b c>)` to `Test`'s `skip` and
dies "was passed a non-integer number of tests". That local test is expected to
retire together with the native provider; this ticket is about the *interpreter*
gap that blocks the roast file.

## Where to look

Three things have to compose, and it is worth checking which of them is
actually missing before writing code:

1. `BEGIN` applied to a `my` declaration statement (not a block).
2. List-assignment destructuring into several `&`-sigilled lexicals at once.
3. `do { use Test; (&plan, ...) }` returning a list of routine objects, with
   `use` inside a `do` block scoping the import to that block.

A quick way to tell them apart is to try each in isolation (`my (&a, &b) = ...`
without `BEGIN`; `BEGIN my $x = 1`; `do { use Test; &plan }`) and see which one
already works.
