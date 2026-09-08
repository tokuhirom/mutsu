# A module named only in a comment no longer loads

mutsu splices several builtin preludes into a compilation unit when its source
*mentions* a name: NativeCall's `Pointer` family and its exported helper
routines (`nativecast`, `cglobal`, `nativesizeof`, `explicitly-manage`,
`refresh`), the parametric `Rational` role, the `IO::Socket` role, the
`trait_mod:<does>` CORE.setting candidates, and the `Metamodel::Naming` /
`Metamodel::Stashing` metaroles. Every one of those gates was a
`source.contains(...)` over the raw program text — so a name written in a
**comment** switched the prelude on:

```raku
# `use NativeCall` registers `GLOBAL::nativecast`.
say defined(::('&nativecast'));   # mutsu: True, rakudo: False
```

Deleting that comment line changed the program's output. A comment is not code,
and the observable was a routine the program never asked for becoming
resolvable at the top level.

The cost was not hypothetical. `t/block-use-keeps-nested-module-imports.t`, the
pin for GH #7580, passed on an *unfixed* binary purely because its header
paragraph named the provider its fixtures import: the prelude the test existed
to observe was being installed by the prose describing it. The file carried a
`NOTE:` paragraph warning readers not to name the provider in a comment; that
warning is gone now, and so is the trap it warned about.

## The fix

`src/runtime/source_code_text.rs` introduces `CodeText`, a view of a
compilation unit's source with `#` comments and Pod blocks removed, and it is
now the only thing the prelude gates can be asked about — they take a
`&CodeText<'_>` rather than a `&str`, so a gate added later cannot quietly go
back to reading raw source. One mechanism, applied to all six preludes, rather
than a per-module carve-out for NativeCall.

Two judgement calls are worth recording:

- **String literals are left alone.** A string *is* code — `require ::('Foo')`
  names its module in one — and recognising every Raku quoting construct
  (`q//`, `qq{}`, heredocs, `«»`) well enough to blank them out would risk
  dropping real code from the view. Prose is the part that is definitionally
  not code, and prose is what is removed.
- **Every ambiguity is resolved toward keeping text.** The comment scan tracks
  single- and double-quoted strings so `say "#"; use NativeCall;` keeps its
  `use`; when a line ends inside a quote — an in-flight multi-line string this
  line-oriented pass cannot see the end of — the line is kept verbatim. Keeping
  too much only preserves the old over-eager behaviour for that one line;
  dropping too much would lose a prelude a real program needs.

A real `use` is untouched, including one nested inside a block or inside a
module body: the prelude remains a whole-compunit splice, gated on the code
rather than on the prose around it.

## The bug it was masking

Removing the masking turned `t/nativecall-helpers-are-not-reexported.t` red,
and the failure was real. mutsu splices NativeCall's helper routines into every
compunit that calls one, registering each under `GLOBAL::`
(`PRELUDE_SUB_TRAIT`). A routine call restores the routine registry on the way
out, and that rollback dropped `GLOBAL::` entries wholesale -- including the
splice a module's own body had received while it loaded. `loaded_modules` is
never rolled back, so the later real `use` was a no-op that could not put it
back, and the module's routine died with "Unknown function: nativecast" ever
after:

```raku
lives-ok { EVAL 'use NativeCallHelperUser; 1' }, 'loads';   # a routine call
use NativeCallHelperUser;
cast-through(Str, Pointer);   # Unknown function: nativecast
```

That file passed only because its own header paragraph names `nativecast`
several times, which gave the *main* compunit a copy of the helper that the
module then found. The same masking, one file over.

`reinstate_module_functions` already puts a loaded module's own routines back
after such a rollback, but deliberately withholds the `GLOBAL::`-qualified ones
unless the caller is the `EVAL` rollback: a file with no `unit module` runs its
body at `current_package() == GLOBAL`, so its `sub foo is export` registers
`GLOBAL::foo` -- indistinguishable from an alias installed *for the importing
scope*, and reinstating those leaked `&bar` past the block in
`{ require NoModule <&bar>; }` (`roast/S11-modules/require.t` test 10).

A prelude splice carries no such ambiguity. It is ambient compunit machinery,
never an import alias, and `registration_sub` already treats it that way when
it declines the block-lexical escape hatch for one. So the `GLOBAL::` keys that
came from a prelude splice are now tracked (`prelude_registered_functions`) and
reinstated either way; every other `GLOBAL::` key keeps the old rule, and the
`require` constraint is untouched.

## Pins

- `t/comment-does-not-load-provider.t` — a header paragraph and a Pod block that
  both name `use NativeCall` and its helper routines, asserting none of them is
  visible. It passes under rakudo too.
- `t/nativecall-prelude-gate-sees-real-use.t` — the other direction: a
  block-scoped `use NativeCall` still brings in `Pointer` and `nativesizeof`.
- Nine unit tests in `src/runtime/source_code_text.rs` cover the stripper
  itself: trailing comments, a `#` inside a string, Raku's identifier-internal
  apostrophe (`don't`), unterminated quotes, delimited and abbreviated Pod, and
  `=finish`.
- `t/module-loaded-in-a-call-keeps-its-prelude.t` — the unmasked bug on its own,
  independent of which comments the file happens to carry.

Closes GH #7611.
