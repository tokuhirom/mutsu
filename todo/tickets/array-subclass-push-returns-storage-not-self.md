# Direct (non-deferred) `push`/`append`/`prepend`/`unshift` on an `is Array` subclass instance returns the backing array, not the invocant

Found while fixing `native-array-push-defer-fallback-broken.md` (ADR-0019 E9-pre). That ticket's
fix made the `nextsame`/`callsame` deferral fallback return the invocant for
`push`/`append`/`prepend`/`unshift`, matching raku (`Array.push` returns `self`). The DIRECT,
non-deferred call — no override at all, or an override method's own final `return` value flowing
straight out — has the same bug and was left unfixed (out of scope for that ticket).

## Divergence

```raku
class MyArr4 is Array { }
my $x = MyArr4.new;
my $r = $x.push(1);
say $r === $x;     # raku: True   mutsu: False
say $r.^name;      # raku: MyArr4 mutsu: Array
say $r;            # raku: [1]    mutsu: [1]  (element content is fine, only identity/type is wrong)
```

Verified against Rakudo v2026.06 on 2026-08-13.

## Where to look

`src/vm/vm_call_method_mut_ops.rs`, the array-backed-instance fast path around line 2342 (as of
the fix above): after `Self::native_array_storage_mut(&mut storage, &method, &args)` mutates
`storage` and `write_back_array_storage_instance` writes the updated storage back into the
instance bound to `target_name`, the code pushes `result` — which for push/append/prepend/unshift
is `native_array_storage_mut`'s return value, the raw backing array
(`Value::array_with_kind(...)`), not the instance. The same fix shape as the deferral-fallback
ticket applies: for these four methods, push the (freshly written-back) instance value instead of
`result`. `write_back_array_storage_instance` already builds that instance and stores it in env
under `target_name` — read it back from there (or have it return the new instance) rather than
re-deriving it.

Likely also affects the "richer" interpreter-fallback branch further down in the same function
(the `call_method_mut_with_values("__mutsu_array_tmp", ...)` path around line 2463), which returns
`result` directly too — check whether that path already returns the interpreter's own instance-vs-
storage value correctly (it goes through `call_method_mut_with_values`, a different code path from
the native fast path, so it may not share this bug) before assuming both need the same fix.

## Why deferred

Small, well-scoped, but distinct from the deferral-fallback ticket it was found alongside (only
scoped to the direct-dispatch fast path, not `nextsame`/`callsame`). No `t/` coverage currently
requires `.push`'s return-value identity on a plain (non-overridden) `is Array` subclass, so it
was not a regression the roast/`t/` suite would have caught — worth a follow-up slice with a
raku-verified pin.
