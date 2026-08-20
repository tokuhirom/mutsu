# `.WHICH`/`.WHY` user overrides now win in every call form

`.WHICH` and `.WHY` are the two MOP pseudo-methods that raku treats as
ordinary, overridable methods -- unlike `DEFINITE`/`WHAT`/`WHO`/`HOW`/`WHERE`/
`VAR`, which stay compile-time reflection macros no matter how a class names
a same-named method. mutsu already got the quoted-literal call form right
(`.'WHICH'()`/`."WHICH"()` with a compile-time-known string), but a bareword
call (`.WHICH`) or a dynamic call (`."$m"()`) silently ignored a class's own
`method WHICH { ... }` / `method WHY { ... }` and returned the native
identity hash / `Nil` default instead:

```raku
class Foo {
    method WHICH { "USER-WHICH" }
}
say Foo.new.WHICH;      # raku: USER-WHICH; mutsu (before fix): Foo|<hash>
my $m = "WHICH";
say Foo.new."$m"();     # raku: USER-WHICH; mutsu (before fix): Foo|<hash>
```

The root cause was two independent, redundant "skip native pseudo dispatch"
mechanisms that both blanket-excluded all eight pseudo-method names from
ever consulting a has-user-method check -- correct for the six genuine
macros, wrong for WHICH/WHY:

- **VM opcode level** (`vm_call_method_ops.rs`, `vm_call_method_mut_ops.rs`):
  the has-user-method gate that sets `skip_native` explicitly excluded
  WHICH/WHY, so a bareword call never even looked for an override. The
  dynamic (`.$m`/`."$m"()`) and hyper (`».WHICH`) call forms had no
  `skip_native` concept at all, so `try_native_method` ran unconditionally
  regardless of quoting.
- **Interpreter level** (`methods_call_dispatch.rs`, `methods_mut_dispatch.rs`):
  a local `is_pseudo_method` flag (built from the same eight-name list) gated
  both the native-fastpath bypass and the `shadows_builtin` check that lets a
  user method shadow the by-name builtin dispatchers -- so even once the VM
  layer declined to run natively, the interpreter's own macro-computation
  arms (e.g. `dispatch_why`) still ran unconditionally for WHICH/WHY.

Fixed by narrowing every one of these exclusion lists to the true six
macros, and adding a single shared guard inside `try_native_method_raw`
(mirroring the existing `render_overridden` pattern used for
`gist`/`Str`/`raku`/`perl`) so every call path that reaches
`try_native_method` -- including the dynamic and hyper forms, which had no
per-opcode gate at all -- declines to compute the native default when the
receiver's class defines its own `WHICH`/`WHY`. The default (non-overridden)
answer for both methods, and the true macro semantics for the other six
pseudo-methods in every call form, are unchanged.

Pinned by `t/which-why-user-override.t` (14 assertions across instance and
type-object receivers, bareword/quoted/dynamic call forms, and a named-
variable receiver), dual-oracle verified against `raku`.
