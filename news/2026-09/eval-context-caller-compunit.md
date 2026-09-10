# `EVAL ..., context => CALLER::` now compiles in the caller's compunit

`EVAL $code, context => $ctx` is supposed to compile `$code` as if it stood at
`$ctx`'s frame. mutsu honoured that for the *package* and for how the snippet's
`return` classifies (ADR-0037 §2.2/§2.3), but not for the one thing the vendored
`Test.rakumod` needs it for: which compunit's imports the snippet can see. The
snippet was compiled inside whichever compunit called `EVAL`, so nothing the
*test file* had `use`d was resolvable from it.

That became visible when the real upstream `Test` module became the default
provider (da21ca091, [#7554](https://github.com/tokuhirom/mutsu/issues/7554)):
its string form of `throws-like` is `EVAL $code, context => $caller-context`,
and the native provider never took that path. `Encode/t/01-basic.t`, a two-line
file whose only assertion is a string `throws-like`, reported

```
# Expected: Encode::X::Encode::Unknown
# Got:      X::AdHoc
# Exception message: Could not find symbol 'Encode::decode'
```

and the bundled-library gate was red on `main` for three whitelisted files.

## What changed

Three things, all of them the same mistake at different depths — code was
attributed to the compunit that *invoked* it rather than the one that *declared*
it.

1. **The EVAL unit's parent.** A `CALLER::` pseudo-stash now carries the
   compunit of the frame it was taken from (`__mutsu_origin_unit`, stamped
   beside the package and routine identities it already carried), and
   `EVAL ..., context => $ctx` parents the EVAL compilation unit on it instead
   of on the ambient one. The `use`-grant walk in
   `Interpreter::qualified_name_visible_here` already follows that parent chain
   ([#7797](https://github.com/tokuhirom/mutsu/issues/7797)), so the snippet
   inherits the caller's imports and nothing else needed to change.

2. **Routines declared while a module's mainline runs.** A module body runs via
   `run_block`, which pushes no routine frame, so `executing_source_file()`'s
   frame walk answered whichever routine was still below it. Loading a module
   from an `EVAL "use ..."` reached inside a routine — which is exactly
   `Test.rakumod`'s `use-ok` — therefore stamped every routine the loaded module
   declares with the *calling* module's file. That in turn anchored the loaded
   module's own `sub EXPORT` to the wrong compunit, and
   `Terminal::ANSI::OO.new` inside `Terminal/ANSI/OO.rakumod`'s own `EXPORT`
   failed the qualified-name gate. Registration now uses
   `executing_source_file_for_module_load()`, the file-level counterpart of the
   existing `executing_unit_sym_for_module_load()` correction.

3. **`END` phasers.** An `EndPhaser` already remembered its declaring *package*
   so a phaser in a `unit module Foo` could still reach `Foo`'s routines by bare
   name at exit; it now remembers its declaring *compunit* for the same reason.
   `Log::Async`'s `END { Log::Async.instance.done if Log::Async.instance }`
   otherwise ran with `current_unit` back at the main script — which, for a
   module pulled in by `use-ok`, never named `Log::Async` at all.

## Result

`DateTime::Parse/01-basic.t`, `Encode/01-basic.t` and `Log::Async/08-use.rakutest`
pass again, and the bundled-library gate for those three batteries is green.
`t/modules/eval-context-caller-compunit.t` pins all three mechanisms, and passes
under `raku` as well.

Log::Async's other non-whitelisted failures (`04-filter`, `10-formatter`,
`12-context`, `14-frame`) are unaffected: they are caller-frame *reporting*
(`callframe`-derived file/line in log records), a separate gap from symbol
visibility.
