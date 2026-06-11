# Element-element / container-leaf binding — implementation plan

> Prep for the next session. Phase 2 Stage 2, the **hardest** remaining slice
> (binding to a *container-valued* element). Read alongside
> `docs/container-identity.md` "調査記録 2026-06-11 (element-element …)".
>
> The 2026-06-11 prototype got **single-level container binds fully working** but
> was reverted because the deep cases need ContainerRef-descent across *all*
> index-assign handlers + cycle-safety + leak hardening — too much for one PR.
> This plan embeds the prototype code so re-implementation is *apply*, not
> *re-derive*, and scopes the remaining work precisely.

## Target tests (the definition of done)

`roast/S03-binding/nested.t` — currently 7 fails: **11-12, 32-33, 35-37**.
This slice targets **32-33, 35-37** (element-to-element binds). 11-12 are a
separate path (`<key>()` mid-path, `IndexAssignGeneric`/method-lvalue).

Minimal repros (write to `tmp/` with the Write tool — `tmp/` is gitignored):

```raku
# A. single-level container bind (PROTOTYPE MADE THIS WORK — regression guard)
my %h = key => { inner => 5 };
my $x := %h<key>;
say $x<inner>;          # 5
%h<key><inner> = 50;
say $x<inner>;          # 50  (prototype: OK)

# B. RHS deep container-leaf bind (prototype: only 1/3 directions worked)
my $foo = [ "ig", { key => { subkey => [ "ig", 2 ] } } ];
my $x := $foo[1]<key>;
say $x<subkey>[1];            # 2
$foo[1]<key><subkey>[1] = 7;
say $x<subkey>[1];           # 7  (prototype: OK after deep-handler descent)
$x<subkey>[1] = 8;
say $foo[1]<key><subkey>[1]; # 8  (prototype: FAILED — root `$x` is itself a cell)

# C. element-element bind (nested.t 32-37 shape — prototype: unsolved)
my $foo = [ "ig", { key => { subkey => [ "ig", 2 ] } } ];
my $bar = [ "ig", { key => { subkey => [ "ig", 5 ] } } ];
$bar[1]<key><subkey> := $foo[1]<key>;
say $bar[1]<key><subkey><subkey>[1];   # 2
$foo[1]<key><subkey>[1] = 7;
say $bar[1]<key><subkey><subkey>[1];   # 7
$bar[1]<key><subkey><subkey>[1] = 8;
say $foo[1]<key><subkey>[1];           # 8

# D. cycle-safety guard (nested.t 268+) — self-referential bind MUST NOT hang
my $struct = [ "ig", { key => { foo => "bar", subkey => [ "ig", 100 ] } } ];
$struct[1]<key><subkey>[1] := $struct[1]<key>;
$struct[1]<key><subkey>[1]<foo> = "new";   # must terminate
```

Reference: `raku tmp/<file>.raku` for expected output before each comparison.

## Root cause (why container leaves need cells)

A bind to a *container-valued* terminal element (`$foo[1]<key>` holds a Hash)
currently keeps a `HashSlotRef`, which goes **COW-stale** when a later deep write
clones the enclosing structure. The fix is the same as scalar leaves — promote to
a shared `ContainerRef` cell — **but only at the terminal (outermost) bind
subscript**, because an *intermediate* index must keep a traversal SlotRef so the
deeper path resolves through the shared inner Arc.

## Phase 1 — terminal-promotion (PROTOTYPE: works; re-apply verbatim)

Distinguish the terminal bind index (promote container leaves) from intermediate
(keep SlotRef). The compiler knows; signal it with a flag + a new op.

### 1a. `src/compiler/mod.rs` — add field + init

```rust
// after `scalar_bind_autovivify: bool,`
/// When true (alongside `scalar_bind_autovivify`), the next Index compiled is
/// the TERMINAL element of the bind RHS (outermost subscript whose value is
/// bound). A terminal index promotes even a container-valued (Array/Hash) leaf
/// to a cell. Cleared while compiling the inner `target` so only the outermost
/// index is terminal.
bind_terminal: bool,
```
Init `bind_terminal: false,` in `Compiler::new()`.

### 1b. `src/opcode.rs` — new variant (after `IndexAutovivifyLazy`)

```rust
/// Like IndexAutovivifyLazy, but the index is the TERMINAL element of a `:=`
/// bind RHS. A container-valued (Array/Hash) leaf is promoted to a
/// `ContainerRef` cell — not kept as a traversal SlotRef.
IndexAutovivifyLazyTerminal,
```
Build was clean (no exhaustive-match breakage) — opcode analysis fns have catch-alls.

### 1c. `src/compiler/expr_data.rs` `compile_expr_index` — capture + clear + emit

At the top (after `let use_autovivify = self.scalar_bind_autovivify;`):
```rust
let is_terminal = use_autovivify && self.bind_terminal;
let lazy_op = if is_terminal {
    OpCode::IndexAutovivifyLazyTerminal
} else {
    OpCode::IndexAutovivifyLazy
};
let saved_terminal = self.bind_terminal;
self.bind_terminal = false;   // inner `target` indices are intermediate
```
Replace each of the three `self.code.emit(OpCode::IndexAutovivifyLazy);` with
`self.code.emit(lazy_op);`. At the end of the fn: `self.bind_terminal = saved_terminal;`.

### 1d. Set `bind_terminal = true` at every bind-RHS entry point

- `src/compiler/stmt.rs` ~767 (the `bind_vardecl` VarDecl branch): wrap
  `self.scalar_bind_autovivify = true;` … `= false;` to also set/clear
  `self.bind_terminal`. **Verified sufficient** for `my $x := %h<key>` (it reaches
  via the `SyntheticBlock`+`MarkBind` loop in stmt.rs which compiles the inner
  VarDecl through this branch).
- AUDIT the other two `scalar_bind_autovivify = true` sites in
  `src/compiler/expr_binary.rs` (~381, ~390): these are `=:=` (ContainerEqRaw).
  **Do NOT set bind_terminal there** — `=:=` must not promote container leaves
  (re-eval over-promotion; same hazard as the #2908 `=:=` regression).

### 1e. `src/vm.rs` dispatch

```rust
OpCode::IndexAutovivifyLazy => { self.exec_index_autovivify_lazy_op(false)?; *ip += 1; }
OpCode::IndexAutovivifyLazyTerminal => { self.exec_index_autovivify_lazy_op(true)?; *ip += 1; }
```

### 1f. `src/vm/vm_var_index_ops.rs` `exec_index_autovivify_lazy_op(terminal: bool)`

- Hash arm: `resolved.hash_slot_ref(&key, terminal)`.
- Add a terminal Array arm BEFORE the existing `Value::Array(..)` arm:
```rust
Value::Array(..) if terminal => {
    if let Some(idx) = Self::index_to_usize(&index) {
        if let Some(slot_ref) = resolved.array_slot_ref(idx, true) {
            self.stack.push(slot_ref);
        } else { self.stack.push(Value::Nil); }
    } else {
        self.stack.push(resolved); self.stack.push(index);
        return self.exec_index_autovivify_op();
    }
}
```
- The eager-op caller of `array_slot_ref` (line ~122) passes `false`.

### 1g. `src/value/mod.rs` — promote container leaf when `terminal`

`hash_slot_ref(&self, key: &str, terminal: bool)`: change the intermediate arm
guard to `Some(Value::Array(..) | Value::Hash(..)) if !terminal =>` (keep SlotRef);
the scalar/`else` arm promotes. `None` stays lazy regardless of terminal.

`array_slot_ref(&self, idx: usize, terminal: bool)`: change the container guard to
`if !terminal && matches!(elem, Value::Array(..) | Value::Hash(..))` (keep
ArraySlotRef); otherwise promote.

## Phase 2 — deep write-through a container cell (PROTOTYPE: partial)

A cell now *holds* a Hash/Array. A deep write (`%h<key><inner> = 50`) must descend
through the `ContainerRef` and mutate the held container **in place** (shared Arc),
else the write hits the `_ => {}` arm and is silently dropped.

### 2a. `src/vm/vm_var_assign_ops.rs` — `assign_into_nested_container` helper

Factor the 2-level inner assign and add a `ContainerRef` arm. Replace the
`match inner_val { Array|Hash|_ }` block in `exec_index_assign_expr_nested_op`
(the `oh.entry(inner_key)` block, ~3788) with `Self::assign_into_nested_container(inner_val, &outer_key, val.clone());`.

```rust
fn assign_into_nested_container(target: &mut Value, outer_key: &str, val: Value) {
    match target {
        Value::ContainerRef(cell) => {
            let mut guard = cell.lock().unwrap();
            Self::assign_into_nested_container(&mut guard, outer_key, val);
        }
        Value::Array(arr, _) => {
            if let Ok(i) = outer_key.parse::<usize>() {
                if Arc::strong_count(arr) > 1 {
                    let ptr = Arc::as_ptr(arr) as *mut Vec<Value>;
                    unsafe {
                        let v = &mut *ptr;          // NB: bind to a var — `&mut (*ptr)[i]`
                        while v.len() <= i { v.push(Value::Nil); }  // trips clippy autoref
                        Value::assign_element_slot(&mut v[i], val);
                    }
                } else {
                    let a = Arc::make_mut(arr);
                    if i >= a.len() { a.resize(i + 1, Value::Nil); }
                    Value::assign_element_slot(&mut a[i], val);
                }
            }
        }
        Value::Hash(h) => {
            Value::hash_insert_through(Arc::make_mut(h), outer_key.to_string(), val);
        }
        _ => {}
    }
}
```

### 2b. `descend_container_ref` helper + deep-handler wiring

```rust
/// Follow `current` through any chain of `:=`-bound container cells, returning a
/// raw pointer to the innermost held value. SAFETY: single-threaded; the pointer
/// from the cell's mutex data stays valid after the transient guard drops.
/// TODO(cycle-safety): bound or visited-set — see Phase 4.
unsafe fn descend_container_ref(mut current: *mut Value) -> *mut Value {
    loop {
        let cell = match unsafe { &mut *current } {
            Value::ContainerRef(cell) => cell.clone(),
            _ => break,
        };
        let mut guard = cell.lock().unwrap();
        current = &mut *guard as *mut Value;
    }
    current
}
```
In `exec_index_assign_deep_nested_op`, at the start of each `for level in 0..depth`
iteration (after `let is_positional = …;`):
```rust
current = unsafe { Self::descend_container_ref(current) };
```

This made single-level container binds (repro A) fully work and repro B's
**element→source** direction work.

## Phase 3 — root-cell descent across handlers (THE REMAINING WORK)

Repro B's reverse direction (`$x<subkey>[1] = 8`, where `$x` is *itself* a cell)
and element-element (repro C) failed because the nested/generic/named handlers
read the root var via `env_mut().get_mut(&var_name)` and expect `Value::Hash`/
`Value::Array` directly — a `ContainerRef` root falls through and the write is
dropped. Add root-cell descent to each handler's root read.

Handlers (`src/vm/vm_var_assign_ops.rs`):
- `exec_index_assign_expr_named_op` (~2234) — root reads incl. ~2168, ~2638,
  ~2781, ~2645, ~3094. `$x[i]` where `$x` is a cell.
- `exec_index_assign_expr_nested_op` (~3632) — root reads ~3777 (Hash block) +
  the array block above it.
- `exec_index_assign_generic_op` (~4056) — stack-target form.
- `exec_index_assign_deep_nested_op` (~3830) — root descent ALREADY handled by 2b
  (level-0 `descend_container_ref`). ✅

Design: a small helper that, given `&var_name`, returns a `*mut Value` to the
root's innermost held container (descending `ContainerRef`), or a way to descend
the `&mut Value` from `get_mut` before the `if let Value::Hash(..)`. Prefer one
shared helper used by every handler's root read to avoid drift.

Also AUDIT the READ side: deep reads through a container cell
(`$x<subkey>[1]` read, `say`) — confirm `resolve_array_entry`/`resolve_hash_entry`
+ the index-read traversal decont `ContainerRef` intermediates. (Repro B's initial
reads worked, so first-level read decont is fine; verify multi-level read.)

## Phase 4 — cycle-safety (MANDATORY before merge)

Self-referential binds (repro D, nested.t 268+) can make `descend_container_ref`
(or the read traversal) loop forever if a cell transitively holds a `ContainerRef`
back to itself. Note: a cell normally holds a Hash/Array (loop stops immediately);
the risk is a cell holding another `ContainerRef`. Add a guard:
- Track visited cell `Arc::as_ptr` in a small `Vec`/`SmallVec`; break on revisit.
- Or bound the descent depth (e.g. 256) and treat overflow as "stop".

The 2026-06-11 prototype's `make test` was left **unconfirmed** (a long run that
may have been slow, not hung — `raku`-less `cyclic.raku` repro D passed in <1s).
**Re-confirm `make test` completes** and time `int.t` (perf cliff canary) before
trusting the change.

## Phase 5 — container-cell leak hardening

Container cells now flow into more paths. Like the Stage-1 `.values` leak (#2908)
and the Stage-2 array-copy leak (#2914), AUDIT + decont `ContainerRef` in:
- `.kv`, `.pairs`, `.antipairs`, `.invert`, `.list`, `.Hash`/`.Array` coercions
- `.raku`/`.gist` rendering of a hash/array containing a bound element
- positional/associative **slices** (`@a[1,2]`, `%h<a b>`)
- `for`/iteration over a hash/array with a bound element
- sort/compare (the original `.values.sort` leak shape)

Each only matters when a bound cell is present, so guard with
`iter().any(is ContainerRef)` to keep the common path zero-cost (the #2914 pattern).

## Verification plan (every iteration)

1. Repros A–D vs `raku` (D must terminate).
2. `roast/S03-binding/nested.t` — target 32-37 pass, **no regression** on 1-31/40-43.
3. `t/element-bind-cell.t` (35 cases) — extend with container-leaf + element-element.
4. `make test` — **confirm it completes** (prototype left this open); only the known
   `native-array-mut.t` #26 Arc-ptr flaky may fail (~38% on main too).
5. `int.t` wall-clock ≈ 0.16s (no perf cliff — terminal-promotion is escape-aware).
6. Push → **CI release roast** (the authoritative leak check; element changes leak
   only in release — #2898/#2908 precedent).

## Known pitfalls (from the prototype)

- `&mut (*ptr)[i]` trips clippy `implicit autoref of raw pointer`. Bind
  `let v = &mut *ptr;` first, then `&mut v[i]`.
- `bind_terminal` must be cleared inside `compile_expr_index` before recursing
  into `target`, else inner indices also promote (breaks deep traversal).
- Container cells must NOT be promoted for `=:=` operands (over-promotion).
- The reverse direction needs the ROOT to be descended (Phase 3) — easy to miss
  because the deep handler already descends but the nested/generic ones do not.
- Re-confirm `make test` actually completes (Phase 4 note).
