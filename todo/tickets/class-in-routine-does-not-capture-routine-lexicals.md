# A class declared inside a routine does not capture that routine's lexicals

A `class` (and by extension its methods) declared in the body of a `sub`/`method`
sees `Nil` where it should see the routine's `my` variables. The same declaration
inside a bare block, or at mainline scope, works.

## Minimal repro

```raku
sub p { my $m = 2; class C { method go() { $m } }; C.new }
my $c = p();
say $c.go;
```

* `raku`: `2`
* `mutsu`: `Nil`

The signature side fails the same way, which is how this was found — a `where`
constraint on a method parameter of such a class rejects every value:

```raku
sub p { my $m = 2; class C { method go($x where $m) { $x } }; C.new }
p().go(2);   # raku: lives.  mutsu: X::TypeCheck::Binding::Parameter
```

## What is (and is not) already fixed

`news/2026-08/where-constraint-declaration-scope-capture.md` fixed the two
*sub-side* channels this shape resembles:

* the compiler now folds a signature's declaration-time reads (parameter
  defaults, `where` constraints) into the compiled body's own `free_var_syms`
  (`Compiler::fold_decl_time_param_captures`), which covers method bodies too
  since they go through `compile_closure_body_with_routine_flag`; and
* a *named sub* escaping as its declaring routine's return value now has that
  routine's live local slots injected into its captured env
  (`Interpreter::inject_frame_locals_for_free_vars`, called from
  `call_compiled_function_named_inner`).

So the compile-time capture *set* is already correct for the method above — what
is missing is the runtime injection on the class-registration path. A method's
env is snapshotted when the class is registered (`exec_register_class_op` /
`vm_register_ops.rs`'s method-capture path), and that snapshot has the same
dual-store blind spot the sub path had: a `my` in the enclosing *routine* body
lives in a local slot, not in `env`, so the flattened capture misses it (or, under
shadowing, captures an outer scope's same-named value instead).

## Where to look

* `src/vm/vm_register_ops.rs` — the method-capture loop around
  `analysis.free_var_syms` / `free_var_parent_slots`, and
  `Interpreter::inject_frame_locals_for_free_vars`, which is the ready-made
  primitive for exactly this injection (it returns the names it installed so they
  can be vouched for as authoritative captures).
* `src/vm/vm_typedecl_ops.rs` — `exec_register_class_op`.
* `src/compiler/helpers_method_body.rs` — `bubble_decl_time_free_reads` /
  `decl_time_param_free_var_syms`, the compile-time half, already in place.

## Why it is a ticket and not a deep item

The compile-time analysis is done and the runtime primitive exists; what is
needed is finding the class/role registration site's env capture and giving it
the same slot injection the sub path got, then deciding whether those names are
authoritative (they are, once the declaring routine's frame has exited — but a
class registered in a routine that is still running is a live frame, so the vouch
needs more care here than it did for a returned sub). Pin any fix with the two
repros above plus a shadowing variant.
