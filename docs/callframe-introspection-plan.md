# CallFrame introspection — implementation plan

Status: **Planned** (investigated 2026-07-21, not yet started). Target: the four
`Type/CallFrame.rakudoc` doc-diff findings, plus the related `.annotations`/`.code`
accessors. This is a **medium feature** (frame modeling), not a quick accessor fix —
see "Why it is not a clean slice" below.

Source of truth for expected behavior is reference `raku` (Rakudo v2026.06). Every
snippet below was run against both `raku` and `target/debug/mutsu` on 2026-07-21.

## What already works

The `CallFrame` instance is built in `src/runtime/system_introspect.rs`
(`callframe_value`) and stores `line`, `file`, `code`, `annotations`, `my`,
`subname`, `package`, `subtype`, `sub`. The accessors dispatch through the plain
Instance-attribute path, so these are already correct:

- `callframe(0).line` → the call-site line (e.g. `5` when called on line 5). ✅
- `callframe(0).file` → `?FILE`. ✅
- `callframe(N)` for `N >= 1` **when a real caller frame exists** (i.e. called from
  inside a sub/block that pushed a frame): returns the caller `CallFrame`. ✅
  `sub foo { callframe(1).line }; foo()` works.
- `callframe(0).annotations<file> eq callframe(0).file` → `True`. ✅
- `.my<$var>` returns lexicals of the frame (modulo raku's own
  `LoweredAwayLexical` optimization on some vars). ✅ enough.

## The gaps (the four findings)

### G1 — `.code` is `Nil` for non-Sub frames

raku models every frame's code by its kind; mutsu returns `Nil` unless the frame is
a named `Sub`.

| context | raku `.code.^name` / `.gist` | mutsu |
|---|---|---|
| inside a named sub | `Sub` / `&foo` | `Sub` / `sub foo () {…}` (gist differs) |
| inside a plain `for {}` block | `Block` / `-> ;; $_ is raw {…}` | `Nil` (`Any`) |
| inside a `FIRST {}` phaser | `Code` / `Code.new` | `Nil` |
| `callframe(1)` at top level | `Mu` | (frame itself is `Nil` — see G2) |

Root cause: `callframe_value` depth-0 uses `current_routine_sub_value()`, which only
scans `block_stack` for a `Sub`. Inline `for`/`while` bodies and phasers execute as
bytecode **without pushing a Code value** onto `block_stack`, so there is nothing to
return. The depth≥1 path already does `code.or_else(|| block_stack.last())`
(`push_caller_env_with_code`, `runtime_caller_env.rs`), so named-sub callers work but
block callers do not.

The Sub-frame `.gist` also differs: raku gives `&foo`, mutsu gives the full
`sub foo () {…}`. That is a general `Sub.gist`-in-code-context question, arguably
separate.

### G2 — no bootstrap/"setting" frame, so `callframe(1)` at top level is `Nil`

raku always has an outer frame above the mainline (the setting/bootstrap). At the
top level:

```raku
# (callframe calls on lines 4 and 5 of the file)
say callframe(1).line;  # raku: 1   ; mutsu: Nil
say callframe(0).line;  # raku: 5   ; mutsu: 5   ✅
```

`callframe(1)` at top level returns a `CallFrame` whose `.line` is **1** and
`.code` is **`Mu`**. mutsu's `callframe_stack` is empty at top level (the mainline
pushes no frame for itself), so `depth > stack_len` → `None` → `Nil`.

**Caution / open question:** the `.line == 1` value is reverse-engineered from two
examples. Before implementing, confirm what line the setting frame reports in more
cases (e.g. `callframe(1)` from a sub that was itself called deep, and
`callframe($n)` for `$n` one past the real stack). Do **not** hardcode `line 1` as a
special case to pass the doc block — establish the general rule first (likely: the
setting frame's own "current line" is the unit's first line, i.e. 1, because it is
where control entered the unit).

### G3 — `.code ~~ Routine` walk + `.package` (finding 2)

```raku
sub calling-frame() {
    for 1..* -> $level {
        given callframe($level) -> $frame {
            when $frame ~~ CallFrame {
                next unless $frame.code ~~ Routine;   # skip Block/Code/Mu frames
                say $frame.code.package;              # raku: (GLOBAL)
                last;
            }
            default { say "no calling routine or method found"; last; }
        }
    }
}
calling-frame;   # raku: (GLOBAL) ; mutsu: "no calling routine or method found"
```

This is a pure consequence of G1+G2: mutsu's frames have `.code == Nil`, which is
not `~~ Routine` and not `~~ CallFrame`-typed enough, so the walk falls to
`default`. Fixing G1/G2 fixes this. Note the `.code ~~ Routine` predicate must be
true for `Sub`/`Method` frames and false for `Block`/`Code`/`Mu` frames — the frame
type modeling from G1 is exactly what drives it. `.code.package` for a mainline sub
is `(GLOBAL)` (the `GLOBAL` package object, gisting `(GLOBAL)`).

### G4 — `.annotations` returns `Hash`, should return `Map`

```raku
say callframe.annotations.^name;   # raku: Map ; mutsu: Hash
```

`build_annotations` (`system_introspect.rs`) returns `Value::hash(...)`. raku's
`.annotations` is an immutable `Map`. This is the **one genuinely small, isolated
fix** here: build a `Map` value instead of a `Hash` (mutsu already has a Map type —
see how other builtins return Map). `annotations<file>`/`<line>` lookups and the
`eq callframe.file` check already pass, so only `.^name` changes.

## Why it is not a clean slice

G1 and G2 are the load-bearing gaps and both require **frame modeling**, not
accessor tweaks:

- G1 needs inline `for`/`while`/`repeat` bodies and phasers to carry a **Code value**
  reachable from `callframe_value` — either pushed onto `block_stack` (perf-sensitive:
  every loop iteration) or threaded via a lighter side channel used only when
  `callframe`/`caller`/`&?BLOCK`/`&?ROUTINE` is actually referenced.
- G2 needs a synthetic **setting frame** at the bottom of the stack with well-defined
  `line`/`code`/`file`, whose semantics must be established generally rather than
  reverse-engineered from the doc block (else it is exactly the "special-case logic to
  pass a test" the project forbids).
- The exact `.code.gist`/`.^name` per frame kind (`Sub`/`Block`/`Code`/`Mu`) must
  match — several distinct code representations.

## Suggested sequencing

1. **G4 first** (small, isolated, low risk): `.annotations` → `Map`. Can even land as
   its own tiny PR independent of the rest.
2. **G1**: give block/phaser frames a Code value.
   - First decide the mechanism: a compile-time flag that makes loop/phaser blocks
     push a lightweight Code marker onto `block_stack` **only** when the body (or an
     enclosing scope) references `callframe`/`caller`/`&?BLOCK`/`&?ROUTINE`/`FIRST`,
     to avoid a per-iteration cost on the hot loop path. Measure with
     `MUTSU_VM_STATS` before/after on the fib/loop benches.
   - Model the returned value so `.^name` is `Block` for loop bodies and `Code` for
     phasers, with the right gists (`-> {…}` vs `Code.new`).
   - Fix `current_routine_sub_value()` (depth-0) to fall through to this block value,
     mirroring the depth≥1 `block_stack.last()` fallback.
3. **G2**: model the setting frame. Establish the general `line`/`code` rule with more
   raku probes first. Consider pushing a real bottom "mainline"/"setting" entry at
   interpreter start instead of special-casing `depth == stack_len + 1`.
4. **G3** falls out of G1+G2; add a `.code ~~ Routine` regression test
   (`Sub`/`Method` true; `Block`/`Code`/`Mu` false) and the `.package == GLOBAL` walk.

## Repro corpus (write these as `t/callframe-*.t` when implementing)

- `t/callframe-code-kinds.t` — `.code.^name` for sub / for-block / FIRST-phaser / top.
- `t/callframe-setting-frame.t` — `callframe(1).line == 1`, `.code ~~ Mu` at top.
- `t/callframe-routine-walk.t` — the finding-2 `calling-frame` walk → `(GLOBAL)`.
- `t/callframe-annotations-map.t` — `.annotations.^name eq 'Map'` (G4).

## Key files

- `src/runtime/system_introspect.rs` — `callframe_value`, `insert_callframe_code_attrs`,
  `current_routine_sub_value`, `build_annotations`, `build_lexical_hash`.
- `src/runtime/runtime_caller_env.rs` — `push_caller_env_with_code` (frame push, the
  `code.or_else(block_stack.last())` fallback), `pop_caller_env`.
- `src/vm.rs` — `VmCallFrame`, `CallFrameEntry`; the `PushBlockFrame`/`PopBlockFrame`
  opcodes (`opcode.rs`) that already model bare-block backtrace frames — G1 may extend
  these to carry a Code value.
- `src/builtins/methods_0arg/dispatch_core_repr.rs:234` — the CallFrame `gist`/`raku`
  arm (its non-`gist` else-branch currently returns the `.raku` string for *any*
  method; verify accessors do not accidentally route here).
- `src/parser/primary/ident/listop.rs` — `callframe`/`caller` listop parsing and the
  `__callframe_line` hidden arg.
