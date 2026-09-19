# A sigilless bind placed at class-body top level stayed writable across compiler chunk splits

A common Raku idiom shares scratch scalars across a class's methods by binding sigilless terms to
them right at the top of the class body:

```raku
my class Operation {
    my (\x1, \x2, \y1, \y2) := my ($x1, $x2, $y1, $y2);
    has $.x;
    has $.y;
    submethod TWEAK {
        ($x1, $x2) = ($!x.min, $!x.max);
        ($y1, $y2) = ($!y.min, $!y.max);
    }
    ...
}
```

Under mutsu, the first write from `TWEAK` (or any other method) to one of these class-scoped
scratch variables died with `Cannot modify an immutable Int` (or, in the single-variable spelling
`my \x := $x;`, `Cannot modify an immutable Package`) — even though nothing in the source asked for
immutability.

## Root cause

`Compiler::class_body_plan` (ADR-0019 D6-3a) lowers a class body's non-method statements into
independently-compiled "Other" chunks — each one its own one-statement compile with a fresh child
`Compiler`, so a `BEGIN` phaser or a bare expression statement can run standalone at class
registration time. Its flatten step unconditionally split every top-level `Stmt::SyntheticBlock`
into its individual inner statements before handing each one to its own chunk.

A sigilless bind (`my \x := $y`) lowers to exactly such a `SyntheticBlock`:
`[MarkBind, VarDecl, MarkSigilless]`. `Compiler::compile_stmt`'s own `Stmt::SyntheticBlock` arm
scans this list for `MarkBind`/`MarkSigilless` *as siblings* of the `VarDecl` to decide, at compile
time, whether the declaration aliases its RHS's container or snapshots its value — and to emit
`OpCode::MarkSigillessBindSource` accordingly. Splitting the three statements into three
independent chunks compiled from different `Compiler` instances threw away that sibling
relationship: the `VarDecl`'s own compile no longer saw the trailing `MarkSigilless`, so it never
emitted `MarkSigillessBindSource`. At run time, `OpCode::MarkSigillessBind` then fell back to
inspecting the already-stored (and already-dereferenced) value instead of the compiler's verdict,
found a plain `Int`, and marked the binding permanently readonly.

## Fix

`class_body_plan`'s flatten step now keeps a `SyntheticBlock` intact — compiling it as one atomic
unit, exactly as `Compiler::compile_stmt` would for any other block — whenever it contains one of
the markers that need sibling context: `MarkBind`, `MarkSigilless`, `MarkSigillessReadonly`, or the
`@`-bind marker call. Every other top-level `SyntheticBlock` (the common case: a grouped declaration
like `my ($a, $b);`) still gets split apart as before, since its individual statements are
independently valid Chunks.

Found and pinned (`t/vm/sigilless-bind-class-body-scratch-var.t`) while investigating why zef's
[Math::Interval](https://raku.land/zef:librasteve/Math::Interval) 0.0.3 — locked on the ecosystem
lock board, [tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884) — regressed on
every arithmetic operator its test suite exercises. The distribution's own remaining failures trace
to a separate, unrelated dispatch bug filed as
[#8807](https://github.com/tokuhirom/mutsu/issues/8807) (a user class inheriting from a built-in
type, with a `FALLBACK` method, misroutes an inherited native method call into `FALLBACK`), so the
ledger record stays `red` for now.
