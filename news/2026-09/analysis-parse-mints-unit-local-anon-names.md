# An analysis-only parse mints unit-local anonymous names

ADR-0065's S0 probe measured the one thing that grew without bound when a
resident process re-parses the same document: each anonymous declaration minted,
interned and permanently leaked one fresh registry name per parse. Interned
strings are leaked for the process lifetime by design, so a language server
holding one file with a `class { }` open through a long editing session leaked a
few megabytes.

Re-measured on the current build before touching anything (8000 iterations,
debug):

| probe | before | after |
| --- | --- | --- |
| `analysis::check` | +8000 interned (1.00/check), 0.479 KiB/check | **+0 interned (0.00/check)**, 0.316 KiB/check |
| plain `parse_source` | +8000 interned (1.00/parse) | +8000 (1.00/parse) — unchanged, and correct |

## The rule, and why it is only a rule for analysis

`ANON_CLASS_COUNTER` / `ANON_ROLE_COUNTER` / `ANON_SUBSET_COUNTER` and
ADR-0047 D1's `CLASS_DECL_ID_COUNTER` are process-global because two
declaration sites in two different compilation units must never collide in a
process-global registry — and `next_anon_role_name` is deliberately shared with
the runtime's `but`-mixin path so a mixed-in anonymous role and a parsed
`role { }` cannot render the same `<anon|N>`.

An analysis-only parse never registers a type: nothing executes, no `ClassDef`
reaches the registry, and the names exist only to be printed back as
`documentSymbol` entries. So *for that mode only* the uniqueness requirement
drops from process-global to compilation-unit-local.

`crate::anon_names` holds that mode: a thread-local set of counters, switched on
by `with_unit_local_names` for the duration of an analysis parse and off for
every other caller. It is deliberately **re-entrant** — a nested call keeps the
outer unit's counters, so a sub-parse within one document cannot restart the
numbering and hand two of its declarations the same name — and it restores the
previous mode through a `Drop` guard rather than trusting the body not to panic.

The ticket's own warning is worth repeating: this is NOT "reset the counters per
parse". That would let two declaration sites in two different units collide in
the shared registry, which is the failure the counters exist to prevent. The
unit-local mode is safe precisely because nothing it names is ever registered.

Three entry points activate it: `analysis::check`, its `recovered_parse_errors`
second pass, and `analysis::symbols`.

## Coverage

`tests/long_lived_parse.rs`:

- `repeated_analysis_of_an_unchanged_document_is_stable` now asserts **zero**
  interned growth, not "at most one per anonymous declaration per pass".
- `repeated_symbol_outlines_of_an_unchanged_document_intern_nothing` — new, for
  the other entry point, which a server calls on every keystroke.
- `a_plain_parse_still_mints_process_global_anonymous_names` — new, pinning the
  other half of the rule: the mode must stay off for every existing caller, so a
  plain parse's counters still advance once per parse.
