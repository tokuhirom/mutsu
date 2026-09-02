# An analysis-only parse should not mint process-unique registry names

ADR-0065's S0 probe (`tests/long_lived_parse.rs`, 2026-09-03) measured the one thing that
grows without bound when the same document is parsed over and over in a resident process:
**each anonymous declaration in the document mints, interns, and permanently leaks one
fresh registry name per parse.**

Measured over 8000 re-parses of an 1140-byte document on a debug build:

| Document | Interned names | Resident memory |
| --- | --- | --- |
| No anonymous declarations | +0 | +124 KiB (noise — the same as at 2000 parses) |
| One anonymous class | +8000 (1.00/parse) | +3988 KiB (~0.5 KiB/parse, linear) |

Interned strings are leaked for the process lifetime by design (`src/symbol.rs` module
docs), so every one of those names is permanent. A language server holding one file with a
`class { }` open through a long editing session leaks a few megabytes. Not fatal — S0
passed on this — but it is the only unbounded component, and it has a clean fix.

## Why the counters are process-global today, and why that is correct for execution

- `ANON_CLASS_COUNTER` / `ANON_ROLE_COUNTER` (`src/parser/primary/misc/anon_decl.rs:10`)
  produce `__ANON_CLASS_N__` / `__ANON_ROLE_N__`, which are the *registry names* an
  anonymous type is stored under. `next_anon_role_name` is deliberately shared with the
  runtime's `but`-mixin path so a mixed-in anonymous role and a parsed `role { }` cannot
  render the same `<anon|N>` id.
- `CLASS_DECL_ID_COUNTER` (`src/ast.rs:34`) produces `decl_id`, which ADR-0047 D1 mangles
  into every lexical class's registry key (`Foo\u{0}<decl-id>`) so that two declaration
  sites never share one.
- `ANON_SUBSET_COUNTER`, `SUPPLY_EMITTER_COUNTER`, and the desugaring temporaries
  (`__with_tmp_N`, `__if_bind_tmp_N`, `__take_value_N`, `__tmp_index_N`, `__anon_state_N`,
  `__anon_array_N`) are the same shape, though most are not interned at parse time.

Resetting any of these per parse would let two declaration sites in two different
compilation units collide in a process-global table. **Do not "fix" this by resetting the
counters.**

## The fix

An analysis-only parse never registers a type: nothing executes, no `ClassDef` reaches the
registry, and the names exist only to be printed back as `documentSymbol` entries. So the
uniqueness requirement drops from process-global to compilation-unit-local exactly when the
parse is for analysis.

Introduce that as a property of the parse entry point the language server uses (ADR-0065
S1 must add one anyway — `dump_ast` returning a `String` is not it), with unit-local
counters in that mode. The mode must be inherited by nested sub-parses within the same
unit, and must be off for every existing caller.

## Why this is a ticket and not a deep finding

It is bounded and needs no new design once S1 has settled what the server's parse entry
point looks like. It should be done *with* that entry point, not before it — retrofitting a
mode flag onto `dump_ast` would be the wrong shape and would have no caller.

## Repro

```
MUTSU_S0_ITERATIONS=8000 cargo test --test long_lived_parse repeated_parse -- --nocapture
```

Compare against the anonymous-declaration-free document in the same file
(`a_document_without_anonymous_declarations_interns_nothing_on_reparse`), which pins the
zero-growth baseline.
