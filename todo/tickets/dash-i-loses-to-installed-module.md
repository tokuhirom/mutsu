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

## Where to look

Module resolution for `use` at run time, and the parse-time scan in
`src/parser/stmt/simple/module_exports.rs` (`find_module_file`, which searches
`LIB_PATHS` — those two must agree on precedence, or the parser and the runtime
can disagree about which file a module is).

Raku's precedence is: `-I` paths (in order) → `RAKULIB`/`MUTSULIB` → installed
repositories (`site`, `vendor`, `core`). mutsu already documents the `-I` over
`MUTSULIB` half in CLAUDE.md; the installed-repo half is what is missing.

## Check while fixing

A precompiled/installed distribution may also be selected by version/auth
criteria (`use NativeLibs:ver<0.0.9>`), which is a separate question from path
precedence — but a plain `use` must still prefer `-I`.
