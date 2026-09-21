# fast_method_cache could cache a constructor that loses a raw-capture alias

While investigating [#8880](https://github.com/tokuhirom/mutsu/issues/8880)
(a method-call perf issue), a prototype change wired `CallMethodMut` into
`fast_method_cache` — the monomorphic per-`(class, method)` dispatch cache
that had previously only ever been consulted by the non-mut `CallMethod`
entry. That prototype surfaced a genuine, previously latent correctness bug
in the cache's shared eligibility gate (`try_populate_fast_cache`), which
this fix closes — independent of whether/when the perf wiring itself lands.

`try_populate_fast_cache` excludes an attributive parameter (`$!x`, which
mutates `self`) from the fast cache, but not a **sigilless raw-capture
parameter** (`\x`), which binds the **argument's own container**, not a
copy. A constructor shaped like:

```raku
method !SET-SELF (\v) { $!x := v; self }
method new       (\v) { self.bless!SET-SELF: v }
```

called repeatedly (whichever entry populates the cache) loses the alias
from the second (cache-hit) call onward: the built instance's `$!x` stops
tracking the caller's container. This is exactly the shape
`roast/S32-list/skip.t`'s ".skip-all and .push-all on slipping slippy
iterators" subtest exercises via a custom `Iterator`, and it is how this was
found — prototyping the `CallMethodMut` wiring made a bareword-receiver
constructor call (`Type.new: $x`, which always compiles to `CallMethodMut`)
reach the fast cache for the first time, and this bug was waiting there.

Fixed by excluding `pd.sigilless` params from the shared eligibility gate.
Also hardened: `has_attr_aliases` (the check that already special-cases a
*declared* attribute alias, `has $x`) now also scans attribute *values*
(`is_container_ref`) so a *runtime*-bound alias is caught too, not just the
declared form.

Regression coverage:
`t/routines/signature/fast-cache-sigilless-param-alias.t`.

The `CallMethodMut` fast-cache wiring that surfaced this is **not** shipped
here — it turned out to have more than one such gap (a second one showed up
in `Cro::HTTP`'s route-delegation tests, involving cached state leaking
between calls in a different way), and chasing each one individually was
not converging on something safe to ship as a bounded slice. #8880 stays
open, un-narrowed, for whoever picks up that wiring next with a fuller
audit of `try_populate_fast_cache`'s eligibility gate against the broader
set of method-body shapes `CallMethodMut` exercises.
