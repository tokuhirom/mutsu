# The sound multi-resolution cache now serves the vendored `Test`'s assertions

The vendored upstream `Test` module answers every assertion with Raku-level
code, so the per-assertion cost is what decides whether
`todo/deep/vendor-real-test-module.md` can flip the default provider. A
callgrind run of 300 `ok 1, "x"` assertions under `MUTSU_REAL_TEST=1` put
**1.06 M instructions on a single assertion**, and 29% of the whole program in
`resolve_function_with_types` — the full multi-candidate walk, re-run on *every*
call of `ok`, `is` and `is-deeply`.

The sound multi-resolution caches (`func_multi_resolve_cache` and its method
twin `multi_resolve_cache`) exist precisely to avoid that walk: they memoize the
winner per `(package, name, argument-type keys)` whenever the candidate set is
type-deterministic. Three separate things kept every `Test` assertion out of
them. All three are now fixed, and none of the fixes is `Test`-specific.

## 1. The synthetic callsite-line marker refused the whole key

mutsu's parser appends a `"__mutsu_test_callsite_line" => <line>` named
argument to every test-assertion call, so a failure can name the line. It is a
diagnostic carrier, not a dispatch participant — `bind_function_args_values`
filters it out before binding and no signature can declare it, the name being
reserved.

`multi_arg_type_keys` did not know that: it refuses to key *any* `Pair`
argument, so the marker made every assertion call un-keyable and the cache was
never consulted. It now keys the marker as a constant, which keeps
"marker present" and "marker absent" in distinct buckets while letting the rest
of the argument list be keyed normally. `ok 1, "x"` in a 20 000-iteration loop
went from 301 full resolves per 300 calls to 2 in total, and the loop from
**5.07 s to 3.58 s**.

## 2. A `:D`/`:U` smiley disqualified the whole name

`func_multi_dispatch_type_cacheable` treated any `:` in a type constraint as
value-dependence, which is right for a coercion (`Int(Str)`) or an enum-value
refinement but wrong for a smiley: `Mu:D` tests exactly `value_is_defined`, a
property the key can carry. It could not carry it before — a type object `Int`
and the instance `42` both key as `Int` — so the refusal was correct as things
stood.

`multi_arg_type_keys` now appends a reserved marker after the type key of an
*undefined* argument, and a trailing smiley is no longer counted as
value-dependent. That admits upstream `Test`'s smiley-split assertions:

```raku
multi sub is(Mu $got, Mu:U $expected, $desc = '') { ... }
multi sub is(Mu $got, Mu:D $expected, $desc = '') { ... }
```

plus the four `is-deeply` candidates, which are split on `Seq:D`.

The two cacheability gates (method-side and function-side) had drifted into two
copies of the same constraint analysis; they now share
`type_constraint_is_value_dependent`, so they cannot disagree about the key
shape they are guarding.

## 3. A variable argument refused the key outright

A `VarRef` — what a variable passed as an argument arrives as — dispatches on
the *source variable's declared type* as well as on the value's own type, so
that `my int $y` and `my $x` holding the same `Int` can pick different
candidates (roast `S06-multi/by-trait.t`). `multi_arg_type_keys` responded by
refusing to key the call at all, which is most of roast: `is $got, $expected,
"..."` and `is-deeply @a, @b, "..."` are the ordinary spellings.

It now keys the declared type too, behind its own reserved marker so a
declared-type key can never be read as the value key of an extra argument. The
only other thing a `VarRef` argument decides is whether an `is rw` parameter
accepts it, and a candidate set containing an `is rw` parameter is already
refused wholesale by both cacheability gates.

## Result

`roast/S03-buf/write-int.t` — the one remaining timeout in the roast
real-`Test` sweep, and the file that runs ~93 000 assertions — went from
**184 079 full multi resolves to 22 805**, and from **48.0 s to 28.6 s**
against a 30 s budget (the native provider runs it in 4.3 s). The
20 000-assertion `ok` loop went **5.07 s -> 3.36 s**.

What is left of those 22 805 resolves is per-*subtest*, not per-assertion:
`_pop_vars` (7 590), `plan` (5 062), `item` (5 060) and `subtest` (5 060), for
the file's 5 060 subtests. `ok`, `is` and `is-deeply` together now account for
8.

Two properties of a *value*, not of an argument list, had to join the key for
this to stay correct, and both were caught by the existing `t/` suite rather
than by reasoning:

* an **enum member** refines within one `value_type_name` (`Less` and `More`
  are both `Order`), so unwrapping a `VarRef` around one made the two share a
  bucket. Enum values now key on `Type::Member`, the same treatment the
  `Package` arm already gets
  (`t/anonymous-any-multi-dispatch.t`).
* an **invocant smiley** (`multi method gist(Cook:U:)` / `(Cook:D:)`) selects
  on the receiver's definedness, which lives outside the argument list the key
  is built from. The method-side key now carries it
  (`t/multi-method-invocant-definedness.t`).

Pinned by `t/multi-resolve-cache-keys.t`, which interleaves the two arms of
each split so a key that dropped either property is served the wrong winner on
the second iteration rather than accidentally passing on a cold cache.

One rakudo divergence surfaced while writing that pin and is filed separately:
a bare integer literal picks the `Int` candidate where rakudo picks `int`
(`todo/tickets/native-int-candidate-loses-to-int-for-a-literal.md`). It is
pre-existing and cache-independent.
