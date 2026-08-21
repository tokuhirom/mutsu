# `.^set_name` on a builtin type's `.WHAT` (`Hash`, `Array`, ...) is a no-op

## Symptom

```raku
class Foo {}
say Foo.^name;               # Foo
Foo.^set_name("Foo(restricted)");
say Foo.^name;                # mutsu: Foo (unchanged) -- raku: Foo(restricted)
```

`methods_classhow_dispatch.rs`'s `"set_name"` handler (~line 190) DOES have a
branch for `ValueView::Package(name)` that writes into
`self.type_metadata.entry(name.resolve()).or_default().insert("__set_name__", ...)`
— so the write itself isn't silently dropped at that call site. The read
side (`.^name`) apparently does not consult `__set_name__` for a `Package`
value the way it does for a `Mixin`'s `__mutsu_type_name__` override (see the
same file, the `ValueView::Mixin` branch just above). Not fully root-caused
in the investigating session — worth tracing `.^name`'s implementation
(likely `methods_classhow_dispatch.rs` or a metamodel-name helper) to confirm
whether it reads `type_metadata`/`__set_name__` at all, and if so why the
repro above still shows the old name.

## Why this matters

`Hash::Restricted`'s custom `is restricted` trait calls
`v.var.WHAT.^set_name("$name(restricted)")` to give the restricted hash's
type a distinguishing display name (`%h.^name.ends-with('(restricted)')` is
one of its own test assertions). `v.var.WHAT` for a `%h` variable is a
`Package("Hash")` value (the shared builtin type), not a fresh per-instance
anonymous type the way Rakudo's actual metamodel gives a freshly role-mixed
object — so even if `.^set_name`/`.^name` round-tripped correctly here, it
would be renaming the SHARED global `Hash` type for every hash in the
program, not just this one. That's a second, deeper issue: mutsu's `Mixin`
representation does not appear to give a role-mixed native value's `.WHAT` a
distinct anonymous type object per Rakudo's actual composition semantics — it
just returns the base type's own `Package` value. Confirming/fixing that is
likely the more correct (if larger) fix than making `.^set_name` on a shared
`Package` "work" (which would be actively wrong — a global rename with
same-process-wide effect).

## Discovered via

Investigating `todo/deep/trait-mod-does-not-callable-sub.md` (now resolved —
see `news/2026-08/trait-mod-does-callable-sub.md`), getting `Hash::Restricted`'s
32-subtest suite running as far as possible. Blocks 2 of its subtests
("is the name changed ok" ×2, one per `%h1`/`%h2` case) — cosmetic relative
to the dist's core restriction behavior, which does not depend on the name.

## Next steps

1. Trace `.^name` for a `Package` value to confirm whether/why it ignores
   `type_metadata`'s `__set_name__` entry.
2. Decide whether `Mixin`-over-native-value's `.WHAT` should return a fresh
   per-instance anonymous type object (matching Rakudo) rather than the
   shared base `Package` — if so, `.^set_name` naturally becomes correct as
   a side effect once `.WHAT` returns something instance-specific to rename.
   This second part is likely the deeper, real fix and may deserve its own
   `todo/deep/` write-up once traced further.
