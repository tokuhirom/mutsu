# Battery: the `if` pragma — conditional module loading

**Slot:** conditional `use` (`use Foo:if(EXPR)`) · **Chosen:** `if` (upstream
`zef:raku-community-modules`, v0.1.5, Artistic-2.0) · **Kind:** Bundled, run
verbatim (`modules/if/`) · **Decision record:**
[ADR-0098](../adr/0098-if-pragma-actions-slang.md)

## What it is

`if` is the Raku port of Perl's `if` pragma: it makes `:if(EXPR)` on a `use`
statement a load condition, so a distribution can pick a backend without
`require` gymnastics.

```raku
use if;
use Crypt::Random::Win:if( $*DISTRO.is-win);
use Crypt::Random::Nix:if(!$*DISTRO.is-win);
```

The pragma is lexical to the compilation unit that says `use if;` — an `EVAL`
string, or a module that merely imports one that uses it, does not inherit it.
Without the pragma, `:if(...)` is just an unrecognized `use` adverb: it selects
nothing and the module is loaded anyway.

It is one 93-line file with **no dependencies**, and eighteen distributions in
the `ecosystem/` parity corpus depend on it — among them `Cro::HTTP` and
`Cro::WebSocket` (through `Crypt::Random`), which puts much of the web half of
the ecosystem behind it.

## Why it is bundled

`Crypt::Random` is already a battery and a hard `Cro::HTTP` dependency, and its
very first two lines are `use if;` followed by two `:if` adverbs — so a mutsu
that ships `Crypt::Random` has to answer for the pragma one way or another.
mutsu used to answer natively: `use if;` compiled to nothing and `:if(...)` was
honoured unconditionally. That is a rung-3 native provider of an ecosystem
distribution, banned by [ADR-0096](../adr/0096-batteries-adoption-policy.md),
and it diverged from Rakudo in both directions. Bundling the real module and
growing the interpreter until it runs (rung 2) removes the divergence and the
private dialect at once.

Alternatives were not weighed: this is not a contested slot. `if` *is* the
mechanism the corpus uses, there is no competing distribution, and the
"alternative" was the native provider this bundling deletes.

## How the verbatim module runs under mutsu (ADR-0098)

`if`'s `sub EXPORT` branches on `Raku.legacy` — which compiler frontend is
compiling it. mutsu answers `False` (the RakuAST-shaped branch), because the
compile-time surface it offers a module is `$*LANG` with
`define_slang`/`slang_grammar`/`slang_actions` (ADR-0026, ADR-0091) and there
is no `$*W` World to mix a role into. That branch registers a slang whose
**actions** role overrides one production:

```raku
$LANG.define_slang('MAIN',
  $LANG.slang_grammar('MAIN'),
  $LANG.slang_actions('MAIN').^mixin(Actions)   # method statement-control:sym<use>
);
```

ADR-0026 read only the grammar half of a `define_slang` call; mutsu now reads
the actions half too, where an override is a **method** (an actions class has
one method per production) rather than a `token`. The name
`statement-control:sym<use>` maps onto the unit-scoped `use_if_adverb` parser
mode, exactly as `term:sym<identifier>` maps onto Tuxic's spaced-call mode. The
method *body* — `RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE`, `$/.panic`,
`nextsame` — is Rakudo's own actions surface and is never executed, per
ADR-0026 §4.

Pinned by `t/modules/import-export/use-if-pragma.t`, which passes 9/9 under
`RAKUDO_RAKUAST=1` as well (8/9 under rakudo's legacy frontend — the one
difference is the deliberately mutsu-specific `Raku.legacy` assertion).

## Status / limitations

- The `if` upstream suite is **1/1 files** in the release gate: `t/if.rakutest`
  passes 5/5 and is whitelisted. It reached that only once a module load stopped
  discarding its writes to the *caller's* dynamic variables
  ([#8229](https://github.com/tokuhirom/mutsu/issues/8229)) — the file counts
  loads by having the loaded module's `sub EXPORT` increment
  `$*PACKAGE_LOADED`. That gap was unrelated to the pragma: a module *mainline*
  write was lost the same way.
- **The `:if` value is evaluated at run time, not at BEGIN.** Rakudo replaces a
  false `use` with `RakuAST::Statement::Empty` at compile time; mutsu still
  scans the named module at parse time (registering its exports) and only skips
  the load. Every consumer shape in the corpus is a platform or
  compiler-version test that answers the same either way. See ADR-0098 §3.
- Any future slang that registers through an actions role gets the surface for
  free; only its overridden production names need adding to the recognized map
  (`parser/stmt/simple/slang_modes.rs`), and an unrecognized one fails loudly.

## Provenance and re-vendoring

| | |
| --- | --- |
| Upstream | <https://github.com/raku-community-modules/if.git> |
| Version | 0.1.5 |
| Commit | `34423f51486692f02dcf71f5129c1959cedbb0a2` (tag `0.1.5`) |
| License | Artistic-2.0 (`modules/if/LICENSE`) |
| Author | Tobias Leich, `auth<zef:raku-community-modules>` |
| Bundled at | `modules/if/` (resolved as `modules/if/lib`) |

Re-vendoring recipe (BATTERIES.md §3):

```sh
git clone https://github.com/raku-community-modules/if.git /tmp/if
git -C /tmp/if checkout <new tag>
rsync -a --delete \
  --exclude '.precomp' --exclude '.META' \
  /tmp/if/lib /tmp/if/LICENSE /tmp/if/META6.json /tmp/if/README.md \
  modules/if/
```

Then: update the version/commit above and the
[bundle index](../../BATTERIES.md#7-bundle-index); bump the `if` row's `commit`
in `batteries.lock` to the same tag and re-run
`scripts/battery-testsuite.sh --update`, reviewing the whitelist diff; and
smoke-test with

```sh
./target/debug/mutsu -e 'use if; use Totally::Nonexistent:if(False); say "ok"'
```

Updates reach users through `mzef` (a newer installed `if` shadows the bundled
copy) as well as through a mutsu release.
