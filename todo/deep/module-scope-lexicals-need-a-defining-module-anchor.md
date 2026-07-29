# A module's file-scope lexicals need a defining-module anchor at the read site

`module_scope_lexicals` (see
`news/2026-07/module-type-aliases-outlive-the-requiring-frame.md`) records the
file-scope names a module declares for itself and resolves them against the
package of the **running frame** — the method's class, then the routine frame's
package, then the current package, each walked up its `::` chain.

That anchor covers a routine of a class the module declares. It does **not** cover
a module that declares no class of its own and whose plain `sub`s are called from
another module's code. There is simply no frame identifying the defining module at
the read site.

## Where it bites

`DBIish`'s `$dbh.prepare(...)`:

```
Cannot dereference a Pointer[Any]: not a type NativeCall can read
```

`DBDish::mysql`'s prepared-statement path binds parameters through
`NativeHelpers::Blob`'s `pointer-to`, which goes through `BODY_OF` in the bundled
`MoarVM::Guts::REPRs`:

```raku
constant intptr is export = ptrsize == 4 ?? uint32 !! uint64;
my %known-bodies = (VMArray => MVMArrayB, CArray => CArrayB, CStruct => CStructB);
sub BODY_OF(Mu \any) is export {
    my \type = %known-bodies{any.REPR};
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
```

This is **not** an unsupportable MoarVM-guts read: mutsu deliberately synthesises
those REPR bodies (ADR-0015 P2, `src/value/value_buf_repr.rs`), and
`pointer-to($buf)` returns a correct pointer when the module is `use`d at file
scope. It fails only on the `require`-inside-a-method route, because
`%known-bodies` (and `intptr`) resolve to nothing and `type` lands as `Any`.

Confirmed with `rust-gdb` at the lookup: the routine stack at that moment is
`DBDish::mysql::StatementHandle::BUILD` and its callers — `MoarVM::Guts::REPRs`
appears nowhere, and `def_file` is `None` on every frame. The table *does* hold
the names, keyed `MoarVM::Guts::REPRs`; nothing at the read site can reach that
key.

## Why this is deep

The fix is a **defining-module register**: while a routine whose body came from
module M runs, M has to be knowable at any name lookup. Candidate designs, none
of them a slice:

- Push the defining module onto a stack on every routine call. Requires a
  `source_file`→module map and a push/pop on the hottest path in the interpreter;
  the cost has to be measured, and it has to cover both the compiled and the
  interpreter dispatch routes (`RoutineFrame::def_file` is populated on only one
  of them today).
- Bake the module into the routine's `CompiledFunction`/`FunctionDef` and read it
  from a "currently executing routine" register. Cheaper per call, but there is no
  such register today, and closures/blocks inside module subs need to inherit it.
- Capture the module's file-scope names into each of its routines at registration
  (a real closure env for registry subs). The most principled, and the largest —
  it overlaps the dual-store work in ADR-0001.

An unprincipled shortcut exists — resolve a name that is unique across all loaded
modules' tables — and is deliberately **not** taken: it would resolve a genuinely
undeclared name in user code to some unrelated module's private lexical.

## Repro

```
cd tmp/dbslot/DBIish-0.6.8
../../../target/debug/mutsu -I lib -I ../NativeLibs-0.0.9/lib \
    -I ../NativeHelpers-Blob-*/lib ../../dbiish-prep.raku
```

(needs the `mutsu-mariadb` container on port 13306). Connecting and `.execute`
already work end to end against the live server; `prepare` is the first call that
reaches `BODY_OF`. `tmp/blobprobe.raku` is the working file-scope-`use`
comparison, and `tmp/reprlike.raku` + `tmp/modprobe/lib/ReprLike.rakumod` are a
reduced module of the same shape that *passes*, because its sub is called
qualified and so has a routine frame naming its package.

## Impact

Last known blocker on `DBIish`'s real end-to-end mysql path, and the gate on
prepared statements for any NativeCall binding that reaches
`NativeHelpers::Blob`. The three earlier blockers are fixed:
`news/2026-07/rw-sub-proxy-fetch-on-otf-call.md`,
`news/2026-07/closure-capture-beats-same-named-caller-lexical.md`,
`news/2026-07/module-type-aliases-outlive-the-requiring-frame.md`.
