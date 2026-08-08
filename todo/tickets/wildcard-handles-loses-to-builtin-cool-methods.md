# `handles *` (wildcard delegation) loses to built-in Cool/Any methods

`handles *` on a method (or attribute) is supposed to let the class intercept
*any* method call not otherwise defined and forward it to the delegate. In
`raku`, this wins even over a built-in method the object would otherwise
inherit from `Cool`/`Any` — e.g. `.uc`:

```raku
class Forward {
    method inner() handles * { 'hello' }
}
say Forward.new.uc;   # raku: HELLO (forwarded to 'hello'.uc)
```

mutsu instead resolves `.uc` through the normal built-in dispatch path before
ever reaching the wildcard-delegation fallback in
`src/runtime/methods_instance_ops.rs` ("Wildcard delegation (`handles *`) and
FALLBACK method dispatch" block, ~line 1712), so `Forward.new.uc` returns the
upper-cased default stringification of the instance (`FORWARD()`) instead of
forwarding to the delegate's `.uc`.

A method name with no built-in collision (e.g. a custom method name) is
unaffected — wildcard delegation only loses when the target method name also
happens to be a real built-in method on `Cool`/`Any`.

Reproduced identically for a plain `class`-declared wildcard handle (not just
`augment class`), so this is not walker-specific drift (ADR-0019 D3) — it is
a dispatch-ordering bug: the built-in method table is consulted before the
wildcard-delegation fallback, when `raku`'s semantics require the reverse
order for a class that declares `handles *`.

Minimal repro: `/tmp/rk9.raku` in the investigating session — recreate with:

```raku
class Forward4 {
    method inner() handles * { 'hello' }
}
say Forward4.new.uc;
```

Root cause not yet investigated in depth — needs tracing how built-in method
resolution short-circuits before the wildcard-handles fallback runs (likely
in `class_dispatch.rs`/`methods_classhow_dispatch.rs`'s owner/candidate
resolution, ahead of `methods_instance_ops.rs`'s fallback block).
