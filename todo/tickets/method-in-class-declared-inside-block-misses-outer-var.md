# A method inside a `class` declared inside a block misses a free variable read only from the method body

Found while mapping the test matrix for
`todo/tickets/block-capture-misses-free-vars-used-only-by-inner-named-sub.md` (now
`news/2026-08/`, PR fixing the nested-named-`sub` case). The named-`sub` fix does NOT cover this
sibling shape, which is architecturally separate (class/method registration, not `SubDecl`).

## Repro

```raku
my $l = 42;
my &blk = {
    my class Foo {
        method go() { say "l=$l" }
    }
    Foo.new.go;
};
blk();
```

- raku v2026.06: `l=42`
- mutsu (main, and after the nested-named-sub fix): `Use of uninitialized value element of type Any
  in string context.` then `l=`

## Root cause (as far as mapped)

`Stmt::MethodDecl` lowers to a `SubDecl` and compiles via
`Compiler::compile_method_body` (`src/compiler/helpers_method_body.rs`), a completely separate path
from `Compiler::compile_sub_body_with_deprecation` (`src/compiler/helpers_sub_body.rs`) that a plain
nested `sub`/`multi sub` goes through. The just-shipped fix added a `named_sub_free_reads` fold
(`src/opcode.rs`'s `compute_free_vars`, populated from `helpers_sub_body.rs`) that bubbles a nested
named sub's `free_var_syms` into the enclosing block's own capture set — but `helpers_method_body.rs`
never contributes to that field, or any equivalent. A `class` declared inside a block registers via
`RegisterDecl(Class)`, which calls `note_type_body_written_lexicals`
(`src/runtime/runtime_thread.rs`) — a RUNTIME mechanism that (as far as traced) only concerns itself
with lexicals the type body *writes*, not full free-variable capture for the enclosing block's own
closure-env snapshot.

Net effect: a variable referenced only inside a method body of a class declared inside a block is
never added to that block's `free_var_syms`, so it is missing from the block's captured closure env
when the block is invoked as a Callable value elsewhere — the same failure mode as the named-sub
ticket, but reached via class/method registration instead of `SubDecl`.

## Why it's a separate ticket, not folded into the sub fix

- Different compile path entirely (`helpers_method_body.rs` vs `helpers_sub_body.rs`), so the fix is
  a distinct fold, not a one-line follow-on.
- Needs its own shadowing/parameter-exclusion analysis (does a method's own attribute (`$.x`/`$!x`)
  or `self` accidentally get pulled in as a free var? almost certainly excluded already by
  `is_non_lexical_name`/`is_attribute_accessor_name`, but should be verified for the class/method
  path specifically).
- Lower priority than the sub case: a `class` declared directly inside a `{...}` block value (rather
  than at file/package top level) is a much rarer shape in real code than a nested named `sub`.

## Suggested approach

Mirror the fix in `src/compiler/helpers_sub_body.rs`/`src/opcode.rs`: after
`compile_method_body` finalizes a method's `CompiledFunction`, fold its `code.free_var_syms` into
the enclosing class/role registration's contribution to the DECLARING BLOCK's free-var set (the class
decl itself would need an analogous `class_free_reads`-style channel, since a class can contain
multiple methods). Verify against the matrix in `t/closure-capture-nested-named-sub.t` extended with
class/method cases, using unique names per case (see that file's own header comment for why: a bare
`{ ... }` test block masks all-but-the-last case via `OpCode::BlockScope`'s conservative env-sync
gate, so DO NOT wrap cases in bare blocks when writing tests for this).
