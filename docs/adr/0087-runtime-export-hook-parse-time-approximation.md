# ADR-0087: A `sub EXPORT` module's parse-time export set is approximated by its unit-scope routines

- Status: Accepted (implemented)
- Date: 2026-09-11
- Related: [#7881](https://github.com/tokuhirom/mutsu/issues/7881), ADR-0081 (compunit-scoped module import aliases), ADR-0085 (ecosystem test-suite parity measurement)

## Context

mutsu's parser pre-scans every `use`d module file for the names it exports
(`src/parser/stmt/simple/module_exports.rs`). That is what lets an imported name
parse as a listop: knowing `root` is a routine is what makes `root <abcd abce>`
a call with a quote-word argument rather than an infix `<` comparison against a
bareword.

The scan is **static**. It reads `is export` traits out of the module's AST (with
a regex fallback for declarations the best-effort parse drops). A module that
exports through Raku's run-time hook instead —

```raku
my sub EXPORT(*@names) {
    Map.new: UNIT::.grep: { .key.starts-with('&') && !(.key eq '&EXPORT') }
}
```

— carries no `is export` trait anywhere, so the scan finds **nothing** and the
importer's parse learns nothing. Every call shape that needs the parser to know
the name is a routine is then a hard parse error:

| | `use String::Utils; say root <abcd abce>` |
|---|---|
| `raku` | `ab` |
| mutsu (before) | `===SORRY!=== Confused. expected expression after infix operator` |

The parenthesised call `root(<abcd abce>)` parsed and ran correctly, and the
identical listop shape parsed fine for a locally-declared `sub root`, so the
missing piece was precisely "the parser does not know `root` is a routine".

The idiom is not rare. `String::Utils` exports its entire surface this way and
uses the listop shape on eight lines of `t/01-basic.rakutest`, which made the
whole file unparseable — 0 of a 124-assertion baseline
(`ecosystem/dists/S/String--Utils.json`).

The root difficulty is real: the export set is *computed by running the module*,
and rakudo gets to run it because it loads compunits at BEGIN time. mutsu loads
modules at run time, so at the moment the importing file is parsed the set
genuinely does not exist yet.

## Decision

**When a module declares a unit-scope `sub EXPORT`, approximate its export set
with the routines the module declares in its own unit scope, and register those
as imported callables for the importer's parse.**

Implemented in `src/parser/stmt/simple/module_exports/export_hook.rs`, driven
from `scan_module_source`:

1. Detect the hook — a unit-scope `Stmt::SubDecl`/`Stmt::ProtoDecl` named
   `EXPORT`, descending through a `unit module Foo;` wrapper, with a
   line-anchored source regex as the fallback for a declaration the best-effort
   parse dropped.
2. Only then, collect the module's unit-scope routine names (again AST-first,
   source-regex as fallback), excluding `EXPORT` itself and any qualified
   `sub Foo::bar`, which is installed in a package rather than in the compunit's
   lexical scope.
3. Feed them into the same `exports` map the `is export` scan fills, so a real
   `is export` entry — which carries precedence and associativity — always wins
   over the approximation.

### Why a superset is the right shape

The approximation is neither sound nor complete, and that is deliberate:

- It can name a routine the module keeps to itself (the `UNIT::` idiom's own
  exclusion list, `&is-CCLASS` in String::Utils).
- It misses a name a hook synthesises out of thin air rather than drawing from
  its unit scope.

What makes the trade acceptable is that **the set is parse-time knowledge only**.
It lands in `Scope::imported_functions`, whose sole consumer is
`is_imported_function` — every call site of which is a parser decision about
whether an identifier is a routine. Run-time name resolution is untouched: it
still resolves against the import set produced by actually running `sub EXPORT`.

So an over-named routine costs a worse diagnostic (a run-time "undeclared
routine" where a parse error once stood) and can never change the meaning of a
program that runs. An under-named one leaves exactly today's behavior. Against
that, registering nothing — the status quo — makes every such call shape a hard
parse error, which is strictly worse than both.

The gate on "the module declares `sub EXPORT`" is what keeps the
over-approximation confined. A module with `is export` traits is unaffected; the
approximation only ever applies where the scan's current answer is the empty set.

> **Amended 2026-09-11 ([#7939](https://github.com/tokuhirom/mutsu/issues/7939)).**
> The decision here is unchanged, but it now covers a second case, so the
> sentence above no longer holds as written: a module with `is export` traits
> *is* affected, in one bounded way. The scan used to keep only the subs whose
> export trait carried the `DEFAULT` or `MANDATORY` tag; it now keeps every
> `is export` sub whatever tag it carries. The tag filter could not implement
> import semantics in the first place — `register_module_exports` is handed the
> module name and never the importer's tag list, so it could not tell `use M`
> (where a `:extra` sub really is not imported) from `use M :extra` (where it
> is), and guessed wrong for every tagged import. The soundness argument above
> carries over verbatim: the widened set is still parse-time knowledge only,
> and a name the importer's tag list withholds still fails to resolve at run
> time. Pinned by `t/modules/import-export/imported-listop-angle-arg.t` and
> `t/modules/import-export/tag-export-parse-time-only.t`.

## Alternatives considered

### 1. Load modules at parse time (rakudo's model)

The correct fix, and the same prerequisite several other parse-time-knowledge
gaps share. It is also an architectural campaign, not a patch: it means BEGIN-time
compunit loading, a re-entrant interpreter inside the parser, and a decision about
the side effects a module's mainline is allowed to have during a parse. Rejected
*for now* as out of proportion to one parse gap — not rejected as the eventual
answer. This ADR does not block it: when parse-time loading lands, the
approximation here becomes dead code and should be deleted, not preserved.

### 2. Evaluate `sub EXPORT` alone, at parse time

A narrower version of (1): run just the hook. Rejected because the hook is a
closure over the module's mainline — `UNIT::` only has entries once the mainline
has run — so "just the hook" is not a smaller thing than loading the module.

### 3. Fall back at the use site

When an identifier is unknown and is followed by whitespace and a `<...>` that
closes on the same line, prefer the quote-word listop reading. Rejected: it is a
guess made where the parser has *least* information (it knows nothing about the
name), it would fire for genuinely undeclared names and turn a precise parse
error into a confusing run-time one, and it addresses only the `<...>` shape
rather than the underlying "we don't know this is a routine" gap. The decision
above makes the guess at the point where the evidence actually is — the module
file — and every other call shape benefits too.

## Consequences

- `String::Utils`'s `t/01-basic.rakutest` parses, taking it from a 0-assertion
  parse death to 117 passing assertions against a 124 baseline. The residue is
  unrelated run-time bugs, not parsing.
- The parse-time export set for a `sub EXPORT` module is explicitly an
  approximation. Anything that starts to use `imported_functions` for something
  other than a parser decision about routine-ness must revisit this ADR first —
  the soundness argument above is the whole licence for the superset.
- Pinned by `t/modules/import-export/runtime-export-listop-parse.t`, which
  covers both directions: the listop call must parse, and a unit-scope routine
  the hook deliberately withholds must still be unresolvable at run time.
- The scan detects the hook **per module file**, which is the right unit: the
  run-time hook is per-compunit too. That invariant was not held on the run-time
  side at first — a module body runs under `GLOBAL`, so every module's
  `sub EXPORT` registered under the one `GLOBAL::EXPORT` key, and two modules in
  a single load chain that each declared one collided with `X::Redeclaration`
  ([#7947](https://github.com/tokuhirom/mutsu/issues/7947)). `load_module` now
  hides the enclosing compunit's hook for the duration of a nested load
  (`Interpreter::hide_export_routines`, restored only *after*
  `apply_module_export` has consumed the nested module's own), so the two sides
  agree on the unit. Nothing in the approximation above changes: the scan
  already read one file at a time.
