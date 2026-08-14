# `.^lookup`/`.^find_method` return a `Sub`-shaped value, not a `Method` instance, so `Method`-only accessors silently misbehave

Found while scoping ADR-0019 Phase F box F1 item 2
(`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`).

## Repro

```
$ raku -e 'my $m = (42).^lookup("floor"); say $m.is_dispatcher; say $m.multi'
False
0
$ ./target/debug/mutsu -e 'my $m = (42).^lookup("floor"); say $m.is_dispatcher; say $m.multi'
<composed-method:is_dispatcher>
<composed-method:multi>
```

Real Raku prints `False`/`0` (a `Method`'s `is_dispatcher`/`multi` accessors). mutsu prints a bogus
`<composed-method:NAME>` string for *any* unrecognized method call on the value — it never errors
and never returns the right answer.

Also reproduces on a plain user method, not just native ones:

```
$ ./target/debug/mutsu -e 'class A { method foo {} }; say A.^lookup("foo").is_dispatcher'
<composed-method:is_dispatcher>
```

## Root cause

`.^methods`/`.^method_table`/`.can` (`src/runtime/methods_classhow_method_obj.rs`,
`collect_class_methods`/`class_method_table`/`collect_can_methods`) build a `Method`
**`Instance`**-shaped `Value` via `make_method_object_with_owner`, carrying `is_dispatcher`,
`signature`, `candidates`, etc. as real instance attributes.

`.^lookup`/`.^find_method` (`src/runtime/methods_classhow_lookup.rs`, `classhow_lookup`/
`classhow_lookup_impl`) instead build a **`Sub`**-shaped `Value::make_sub` — a callable, not an
`Instance` with `Method`'s attributes.

When an unrecognized method (`is_dispatcher`, `multi`, ...) is called on a `Sub` value, dispatch
falls into the "method calls on callables compose" fallback in
`src/runtime/methods_instance_ops.rs` (~line 2117): "calling `.foo` on a `Sub` means apply `foo` to
the Sub's *return value*". That fallback builds a new composed-callable `Sub` named
`<composed-method:foo>` and returns it — since nothing ever calls it, printing it just shows the
placeholder name, and no error is ever raised.

## Why this is a real (not cosmetic) gap

Any `Method`-only introspection accessor (`is_dispatcher`, `multi`, `candidates` in some cases,
`package`/`name` happen to already work because those are special-cased elsewhere) is unreachable
on the result of `.^lookup`/`.^find_method`, silently returning garbage instead of erroring or
answering correctly. This is exactly the kind of surface ADR-0019 Phase F (F1/F2, "derive
`.^methods`/`.^can`/method MRO views from the resolver/table" — same unification PLAN.md §5 calls
for) is meant to fix, but it is a distinct, smaller bug from F1's native-metadata gap: it's a
representation mismatch (two different "this is a method" `Value` shapes that don't interoperate),
not a missing-data problem.

## Why this is deep, not a quick ticket

Unifying the two representations (making `.^lookup` return the same `Method`-`Instance` shape
`.^methods` does, or vice versa) touches:

- `.wrap` on a `.^lookup` result, which today relies on the `Sub` shape's env-carried
  `__mutsu_lookup_class`/`__mutsu_lookup_method` tags (see `make_method_object_with_owner`'s doc
  comment) to register a wrap chain — a `Method`-`Instance`-shaped lookup result would need the
  same wrap-registration path wired differently (the `Instance` already carries these same tags for
  `.wrap` from `.^methods(:local)`, so this may already be closer to solved than it looks, but needs
  verification).
- Direct callability: `.^lookup("foo")(invocant, args)` presumably still needs to work, which is
  why `.^lookup` returns something callable today — a `Method`-`Instance` is not directly callable
  without dispatch support for calling an `Instance`.
- All existing callers of `classhow_lookup`/`classhow_find_method` that assume a `Sub`-shaped
  result (arity/param inspection, `.wrap`, `.^can`'s reuse in some paths) need an audit.

Best done as part of the F1/F2 design once the native-metadata ground-truth pass
(`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`) lands, since both are about making
introspection surfaces agree with each other and with the canonical dispatch table.
