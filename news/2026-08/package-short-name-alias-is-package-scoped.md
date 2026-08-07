# A `class A::B` no longer binds the bare name `B` globally

Declaring a class or role whose name is already qualified (e.g. `class
Cro::Hdr { }`, or `class C1` compiler-prequalified to `M::C1` inside `unit
module M`) used to install its short name (`Hdr`, `C1`) into the interpreter's
single flat, global `env`:

```raku
class Cro::Hdr { }
say Hdr.^name;   # raku: Undeclared name 'Hdr'   mutsu (before): Cro::Hdr
```

That made the short name visible from anywhere in the process for the rest of
the run, and let it silently clobber an unrelated same-short-name declaration
in a totally different scope:

```raku
class Cro::Hdr {
    my grammar Hdr { token TOP { \w+ } }
    method check(Str $s) { so Hdr.parse($s) }
}
my $s = Supplier.new;
my $b = supply {
    my enum E2 <X Hdr Y>;
    whenever $s.Supply { emit Hdr.key }     # raku: Hdr    mutsu (before): dies
};
react {
    whenever $b -> $v { say $v; done }
    whenever Promise.in(0.3) { $s.emit(1) }
}
```

This was the live blocker behind `Cro::HTTP::ResponseParser` and
`Cro::HTTP::RequestParser`: both declare `my enum Expecting <StatusLine
Header Body>` inside their `transformer` supply block, and
`Cro::HTTP::Header` is a class whose short name is `Header` — the enum member
lost to the global alias.

## The fix

`exec_register_class_op` and `exec_register_role_op`
(`src/vm/vm_typedecl_ops.rs`) no longer insert the short-name alias into
`env`. Instead they record it in `package_type_aliases`, keyed by the
*declaring parent package* (`package_type_aliases["Cro"]["Hdr"] =
"Cro::Hdr"`) — the same table that already carries a module's own `use`-import
short names (`src/runtime/run_modules.rs`). Bareword resolution
(`exec_get_bare_word_op`, `src/vm/vm_var_get_ops.rs`) already consulted this
table via `package_type_alias`/`lookup_in_running_package`, which walks
`method_class_stack` → the running frame's `package` → `current_package()`,
each further walked up its own `::` chain — so no new resolution machinery was
needed, only the write side had to stop leaking into `env`.

The previously-suspected second half — "method dispatch must also anchor
`current_package` to the owner class" — turned out to already be handled:
`push_method_class` unconditionally pushes the owning class onto
`method_class_stack` on every method dispatch (both the slow and fast paths),
and `eval_attr_default_expr` unconditionally sets `current_package` to the
constructing class while evaluating attribute defaults. Both already anchor
the package-chain walk correctly with no further changes, including for the
counter-example that sank an earlier naive fix: `class URI::Path` is declared
at file scope in its own compunit, and `unit class URI`'s own methods and
attribute defaults (`has Path $.path = Path.new`) still resolve bare `Path`
correctly.

Covers both the class and role declaration paths; the analogous built-in-type
shadow guard (`role Cro::HTTP::Middleware::Pair` must not hijack the bare name
`Pair`) is unaffected since `is_builtin_type` is still checked before the
alias is ever recorded.

## Related

`todo/tickets/class-nested-my-class-clobbers-outer-short-name.md` was the
*class-body* half of this problem, fixed earlier (see
`news/2026-08/class-body-type-scope.md`): a type declared in a class body has
its short-name binding restored when the body ends, and nested
classes/roles/subsets are recorded as class-scoped short names so the class's
own methods still resolve them (`t/class-body-type-scope.t`). This was the
remaining *package* half.
