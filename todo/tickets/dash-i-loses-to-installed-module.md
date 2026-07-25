# `-I` does not override an installed module of the same name

A module reachable through `-I` is ignored when a module of the same name is
installed in mutsu's site repository — the installed one wins. In raku, `-I`
takes priority over the installed repositories, which is the whole point of the
flag.

## Repro

```sh
mkdir -p tmp/shadow
cat > tmp/shadow/NativeLibs.rakumod <<'EOF'
unit module NativeLibs;
our sub which-one() is export { "from-dash-I" }
EOF
raku  -I tmp/shadow -e 'use NativeLibs; say which-one()'   # from-dash-I
mutsu -I tmp/shadow -e 'use NativeLibs; say which-one()'   # loads the INSTALLED one
```

With `NativeLibs` installed (it is, as a `DBIish`/`DB::SQLite` dependency),
mutsu loads `~/.local/share/mutsu/repo/site/sources/<id>` and never sees
`tmp/shadow/NativeLibs.rakumod`. The error it eventually reports comes from the
installed source, and the stack frames point into
`~/.local/share/mutsu/repo/site/sources/...`, which is the tell.

## Why this matters beyond the flag

It **silently invalidates measurements**. The `DBIish` survey
(`todo/tickets/dbiish-blockers.md`, `docs/batteries/database.md`) passes
`-I ../NativeLibs-0.0.9/lib` to pin a version, but an installed `NativeLibs`
0.0.8 was being loaded instead — a different file with a differently-shaped
`cannon-name`. Anything concluded about "NativeLibs 0.0.9 on mutsu" from a run
that had an installed copy present is suspect and needs re-measuring.

The same trap applies to every battery survey and to any bug report reduced with
`-I` on a machine that has run `mzef install`.

## Where it is

`Interpreter::find_module_path` in `src/runtime/run_modules.rs`. The ordering
data is already right — `add_default_site_repo` *appends* the site repository to
`lib_paths` as an `inst#` entry, so `-I` paths come first in the vector. The
resolver then throws that ordering away:

```rust
// Check inst# paths (CompUnit::Repository::Installation) first.
let mut inst_candidates: Vec<(std::path::PathBuf, String)> = Vec::new();
for base in &self.lib_paths {
    if let Some(prefix) = base.strip_prefix("inst#") { … }
}
```

It makes a full pass over `lib_paths` collecting every `inst#` candidate and
resolves those before ever looking at the plain directories — the exact
inversion of Raku's precedence. The fix is to walk `lib_paths` **once, in
order**, treating each entry as either an installed repository or a plain
directory, rather than hoisting all the `inst#` ones.

Raku's precedence is: `-I` paths (in order) → `RAKULIB`/`MUTSULIB` → installed
repositories (`site`, `vendor`, `core`). mutsu already documents the `-I` over
`MUTSULIB` half in CLAUDE.md; the installed-repo half is what is missing.

Also check the parse-time scan, `find_module_file` in
`src/parser/stmt/simple/module_exports.rs`, which searches `LIB_PATHS`: the
parser and the runtime must agree on which file a module is, or the parser can
extract exports from one file while the runtime loads another.

## Keep these while fixing

- **Candidate selection within one installed repo.** Several installed dists can
  provide the same short name, and the `use` statement's `:ver`/`:auth`/`:api`
  selectors plus the highest-version tie-break apply *within* an `inst#` entry.
  That is a separate concern from path precedence and should not change.
- **`bundled_lib_paths` stays lowest.** It is deliberately last so that an
  `mzef`-installed version shadows a bundled battery (`run_modules.rs` says so).
  The battery test-suite gate (`scripts/battery-testsuite.sh`) is the check that
  this still holds.
- A plain `use` must prefer `-I` even when the installed copy has a *higher*
  version — the flag is not a version hint.
