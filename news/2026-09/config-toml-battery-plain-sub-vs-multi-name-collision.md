# A same-named plain sub in an unrelated compunit could shadow a module's own `multi sub`

[#7539](https://github.com/tokuhirom/mutsu/issues/7539) tracks bundling
`Config::TOML` v0.1.3 and its dependency `Crane` v0.1.2 as a battery. Bisecting
`Config::TOML::Dumper`'s own internal TOML-value renderer — a `multi sub
to-toml(Str:D $s)` / `multi sub to-toml(Int:D $i)` pair, never exported, used
only inside that one file — turned up a general dispatch bug that was the
dominant blocker for both `Config::TOML` and `Crane` at once.

## The bug

`Config::TOML.rakumod` `use`s `Config::TOML::Dumper` and separately exports
its own **plain** (non-`multi`) `sub to-toml(Associative:D $container, ...)`.
Nothing in `Config::TOML::Dumper`'s own body ever `use`s `Config::TOML` back —
but a bare `to-toml(...)` call *inside* `Dumper` kept resolving to the outer,
unrelated plain sub instead of `Dumper`'s own multi candidates, with no type
check at all:

```raku
use lib 'lib';
use Config::TOML;
say to-toml({a => "x"});
# raku:  a = "x"
# mutsu: Type check failed in binding to parameter '$container';
#        expected Associative:D but got Str
```

Two compounding causes, both general (neither is TOML-specific):

1. **A premature exact-key match.** Resolving a bare call first checks the
   registry for an exact, arity-less `pkg::name` key before ever consulting
   the typed multi candidates. A multi candidate is always registered under
   an arity/type-suffixed key (`pkg::name/1:Str:D`), so this exact-key lookup
   can only ever match a genuine *plain* sub — but it ran unconditionally, so
   whenever a same-named plain sub happened to share a registry package
   bucket with a module's own multi candidates, the plain sub won outright,
   every time, regardless of the call's actual argument types. Hit through
   three separate call shapes: an ordinary bareword call
   (`resolve_function_with_types`), the `.&NAME` postfix form
   (`resolve_function`, `Expr::CodeVar`'s resolution path), and — once one of
   the first two had already mis-resolved once — the VM's "positional
   light-call" cache, which is deliberately type-blind and exists to skip
   dispatch entirely for calls it believes are monomorphic.
2. **A package-blind memo, feeding case 1's guard.** The VM's "does this name
   have multi candidates at all?" probe
   (`has_multi_candidates_cached_sym`) is scope-sensitive — the answer
   depends on which package is asking — but its cache was keyed by the bare
   name alone. The first package to ask (typically the exporting module's own
   mainline scope, where the name genuinely has no local multi candidates)
   cached a "no", and every later, differently-scoped probe for the same name
   reused that stale negative — including from *inside* the module that
   actually declares the multi. That let the type-blind light-call cache
   above treat the call as monomorphic, latch onto whichever candidate
   resolved first, and reuse it for every later argument type at that call
   site regardless of its own type: within one `.map({...})` loop alternating
   `Str` and `Int` values, the *second* value crashed with a binding failure
   against the *first* value's candidate.

## The fix

`resolve_function_with_types` (`dispatch_resolve.rs`) and `resolve_function`
(`resolution.rs`) now skip the exact-key shortcut whenever a multi candidate
is also registered under the name's base, deferring to the typed candidate
walk (falling back to the exact match afterward only when nothing typed
matched — the ordinary "there really is just a plain sub" case is unaffected).
`multi_candidates_cache` (`vm_call_dispatch.rs`) is now keyed by
`(current_package, lexical_package, name)`, the same shape its siblings
`declared_fn_cache` / `multi_fn_cache` already used.

Pinned by
`t/modules/compunit/cross-compunit-multi-vs-plain-sub-name-collision.t`, a
two-module fixture reproducing the exact shape (a `use`d module with private
multi candidates; a using module exporting a same-named plain sub).

## Result

| Suite | raku | mutsu before | mutsu after |
| --- | --- | --- | --- |
| `Config::TOML` v0.1.3 | 19/19 files | 14/19 files, 56/68 assertions | 14/19 files, **60/68** assertions |
| `Crane` v0.1.2 | 15/15 files | 4/15 files | **12/15** files |

`Config::TOML` stays at 14/19 files: `dumper/01-basic` and
`exceptions/02-dumper` now clear this bug but hit a separate one, filed as
[#8503](https://github.com/tokuhirom/mutsu/issues/8503) (a typed array
mutated by `push` inside a `.map({...})` closure argument does not share the
outer container — every TOML array value dumps empty). `grammar-actions/01`,
`02`, `04` are the pre-existing string-equivalence / Rat-precision / dumper
residues already on record in `docs/batteries/toml.md`. `Crane`'s remaining
three files (`in`, `patch`, `transform`) are unbisected.
