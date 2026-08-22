# The deferred vivification token is hash-only — a positional step vivifies a Hash

## Symptom

A deferred bind/lvalue chain that takes a *positional* step over a
not-yet-existent container creates a `Hash` keyed `"0"` instead of an `Array`:

```raku
my %h;
sub g(\c, $i) is rw { return-rw c[$i] }
g(%h<g>, 0) = 'x';
say %h.raku;
# raku:  {:g($["x"])}
# mutsu: {:g(${"0" => "x"})}
```

The same shape through the plain `:=` bind:

```raku
my %h;
my $x := %h<g>;
$x[0] = 'x';
say %h.raku;   # raku {:g($["x"])}, mutsu {:g(${"0" => "x"})}
```

## Root cause

`ValueRepr::HashEntryRef { hash: Gc<HashData>, path: Vec<String>, eager }`
(`src/value/view.rs`, `src/value/mod.rs`) is the token a subscript chain hands
out when the key it addresses does not exist yet. It is *structurally* hash-only:

- the root is a `Gc<HashData>`, so an `Array` cannot be a chain root;
- each path step is a `String` key, so a positional index is stringified;
- `Value::hash_entry_terminal` (`src/value/value_methods_a.rs`) walk-creates
  every intermediate level as `Value::hash(HashMap::new())` unconditionally;
- `Value::hash_entry_write` (`src/value/value_methods_b.rs`) inserts through
  `hash_insert_through`, i.e. into a map.

The VM's chain-extension arm (`exec_index_autovivify_lazy_op` in
`src/vm/vm_var_index_ops.rs`, the `_ if matches!(target.view(),
ValueView::HashEntryRef { .. })` arm) just pushes another key onto `path` — and
it is not even *told* whether the subscript was written `[...]` or `{...}`:
`OpCode::IndexAutovivifyLazy` / `IndexAutovivifyLazyTerminal` carry no
`is_positional` flag, unlike `OpCode::Index { is_positional }`.

The eager side already handles both shapes (`Value::array_slot_ref` /
`ensure_array_child` for arrays, `hash_slot_ref` / `hash_autovivify_cell` for
hashes). Only the *deferred* token is one-sided.

## Why this is `deep/`, not a ticket

Fixing it means generalizing the token itself:

- a root that is either `Gc<HashData>` or `Gc<ArrayData>`;
- a path of `enum Step { Key(String), Index(usize) }`;
- `hash_entry_terminal` / `hash_entry_read` / `hash_entry_write` rewritten to
  create the level *the next step asks for* rather than always a Hash;
- `gc_trace` (`src/value/value_gc.rs`) extended to the new root kind;
- `is_positional` threaded onto the two lazy autovivify opcodes and through
  `Compiler::compile_expr_index`;
- every `ValueView::HashEntryRef { .. }` match site audited (~25 across
  `src/value`, `src/vm`, `src/runtime`).

It also touches values that cross the GC boundary, so it wants the
`gc-value`-classified Miri gate (see `trap-miri-conditionally-gated-can-hide-real-ub`).

## Why it matters

It is the largest remaining blocker for `Crane` (`docs/batteries/toml.md`), the
sole dependency of the selected TOML battery. `Crane::In` has a full set of
`Positional` candidates (`return-rw container[@steps[0]]`) that this makes
unusable; `t/in.rakutest` and `t/set.rakutest` fail on exactly these. The hash
half of the same mechanism was fixed by
[ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md); this is
its array twin, and ADR-0059 §"What still blocks Crane" records it as item 1.

## Minimal repro

```raku
my %h;
my $x := %h<g>;
$x[0] = 'x';
say %h.raku;      # want {:g($["x"])}
```
