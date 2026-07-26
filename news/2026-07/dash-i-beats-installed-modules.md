# `-I` now beats an installed module of the same name

A module reachable through `-I` was ignored whenever a module of the same name
was installed in mutsu's site repository: the installed copy won. In raku, `-I`
takes priority over the installed repositories, which is the whole point of the
flag.

```sh
mkdir -p tmp/shadow
# tmp/shadow/NativeLibs.rakumod: unit module NativeLibs;
#                                our sub which-one() is export { "from-dash-I" }
raku  -I tmp/shadow -e 'use NativeLibs; say which-one()'   # from-dash-I
mutsu -I tmp/shadow -e 'use NativeLibs; say which-one()'   # loaded the INSTALLED one
```

The damage went beyond the flag itself: it **silently invalidated
measurements**. The `DBIish` survey passes `-I ../NativeLibs-0.0.9/lib` to pin a
version, but an installed `NativeLibs` 0.0.8 was loaded instead — a different
file, with a differently-shaped `cannon-name`. The tell was a stack frame
pointing into `~/.local/share/mutsu/repo/site/sources/…`. The same trap applied
to every battery survey and to any bug reduced with `-I` on a machine that had
run `mzef install`.

## Two independent inversions

`Interpreter::resolve_module_path` made a full pass over `lib_paths` collecting
every `inst#` (installed-repository) entry and resolved those *before* it ever
looked at the plain directories. It now walks `lib_paths` **once, in order**,
treating each entry as either an installed repository or a plain directory —
Raku's single repository chain, where a repository that cannot satisfy a request
hands off to the next link instead of ending the search.

That alone was not enough. `add_default_site_repo` *appends* the site repository
to `lib_paths`, but it runs from `Interpreter::new`, i.e. before `main` has added
a single `-I` path — so the site repository was already sitting in front of
everything the user asked for. `add_lib_path` now inserts explicit paths ahead of
that default entry, preserving their relative order.

## The rest of the chain, while we were in there

- **`MUTSULIB` was searched before `-I`**, the opposite of what the flag, the
  help text and `CLAUDE.md` all promised. The env paths were prepended and the
  comment claimed "later paths are searched first" — they are not; the resolver
  takes the first hit. `MUTSULIB` is appended after the `-I` paths now.
- **`use lib` appended**, so it lost to `-I`. Raku unshifts the repository onto
  `$*REPO`'s chain, so a `use lib` outranks `-I` and, within one statement, the
  last-listed path ends up first. `add_one_lib_path` prepends now, matching the
  `$*REPO` chaining it was already doing for an `inst#` spec.
- **`detect_inst_distribution` gave up at the first plain path** (`let prefix =
  base.strip_prefix("inst#")?` inside the loop), so with any `-I` path present —
  the normal case — it never examined the installed repositories at all. It skips
  plain entries now.
- **The parse-time scan** (`find_module_file`) iterated extension-major, so a
  `.rakumod` in a later directory beat a `.pm6` in an earlier one, while the
  runtime went directory-major. The parser and the runtime must agree on which
  file a module is, or the parser extracts exports from one file while the
  runtime loads another; the parser is directory-major now, and skips `inst#`
  entries explicitly instead of probing them as directories.

Resolving installed modules at parse time is still not implemented, so exports of
an installed module are invisible to the parser (they are registered at run time
as before). That is a pre-existing gap, unchanged here.

## Resulting order

Highest priority first: `use lib` (most recently added first) → `-I` (in order) →
`MUTSULIB` (in order) → installed repositories → bundled batteries. A plain `use`
prefers `-I` even when the installed copy has a *higher* version — the flag is
not a version hint. Candidate selection *within* one installed repository (the
`:ver`/`:auth`/`:api` selectors plus the highest-version tie-break) is unchanged
and stays per-repository; `t/use-dist-selectors.t` pins it.

Pinned by `t/lib-path-precedence.t`, which runs the interpreter against a fixture
holding the same module three times — two plain directories and one installed
distribution whose version (9.9.9) beats everything — and checks each rung of the
chain. `scripts/battery-testsuite.sh` (93/93) confirms the bundled tree is still
the floor.
