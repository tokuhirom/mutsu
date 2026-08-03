# Dispatch selects a `Pair (:key(...), :value(...))` candidate

A `Pair` unpacks as a capture with two *named* parts and no positional one —
`(2 => 'x').Capture` is `\(:key(2), :value("x"))` — so `Pair (:key($k),
:value($v))` destructures it, `Pair ($k, $v)` does not, and `Pair (:key($k))`
does not either (it leaves `value` unconsumed). mutsu's binder implemented all
three rules correctly; multi-dispatch *matching* did not, so such a candidate
was never selected:

```raku
multi g(Pair (:key($k), :value($v))) { say "pair k=$k v=$v" }
multi g($other)                      { say "other $other" }
g(2 => 'x');   # rakudo: pair k=2 v=x     mutsu: other 2   x
```

`sub_signature_matches_value` disagreed with `bind_sub_signature_from_value` on
two points:

- **Leftover positionals.** `positional_values_from_unpack_target` reports a
  `Pair` as a one-element positional list — the positional destructure forms
  rely on that, and `bind_sub_signature_from_value` unwraps `key => val` there
  by parameter name. An all-named sub-signature consumes no positional element,
  so that element was always left over and the "unconsumed positional elements"
  check rejected every such candidate. The check now sits out exactly this
  shape: an all-named sub-signature against a `Pair` value.
- **A rename read as a destructure.** A named parameter's parens are either a
  rename (`:key($plan)`, `:die(:$throw)` — plain inner params) or a genuine
  destructure (`:value((:key($d), :value(&t)))` — the inner param carries its own
  sub-signature). Matching recursed into both, so a rename demanded a positional
  element from the candidate; anything but a plain scalar there (a `Pair`, an
  `Array`) failed. The binder already distinguishes the two with exactly this
  test, and matching now uses it.

Rejecting an under-specified named destructure is also now explicit: a Pair's
capture has exactly `key` and `value`, so a sub-signature that names only one of
them leaves the other unconsumed and does not match, mirroring the check already
applied to a `Capture` value.

The narrow gate on the first point matters. Skipping the leftover check for
*every* all-named sub-signature instead broke `|c ()` and `|c (:$me)` — those
destructure a `Capture`, whose positional part is real — and made
`multi wind (|c(:$me))` a live candidate for `wind('d', 'e')`
(`roast/S06-multi/subsignature.t`, "Ambiguous call").

## Why it mattered

`Test::Util`'s `group-of` is this exact shape:

```raku
sub group-of (
    Pair (Int:D :key($plan), Pair :value((Str:D :key($desc), :value(&tests))))
) is export is test-assertion { subtest $desc => { plan $plan; tests } }
```

`user_test_decl_beats_native` asks `args_match_param_types` whether the imported
routine really accepts the call before letting it beat mutsu's native provider.
It answered no, so `group-of` was answered natively even with the real module
loaded — and under `MUTSU_REAL_TEST=1` the native `subtest` and the module's own
then kept separate counters, reporting `# You planned 2 tests, but ran 0` on a
group whose assertions had all passed. The retirement in
`news/2026-08/retired-native-test-util-overrides.md` intended to cover this; the
guard was simply unable to see the signature.

Pin: `t/pair-subsignature-dispatch.t` (passes verbatim under `raku`).
