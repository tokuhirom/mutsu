# A `sub EXPORT` wrapper can no longer be shadowed by the routine it wraps

A module's `sub EXPORT` may `use` another module internally and re-export one of its `is export`
routines wrapped in a closure that injects an extra argument, under the *same* bare name it wraps
(`'&greet' => -> |c { greet |c, :extra } }`). This dependency-injection idiom is common in the
ecosystem — `Deps.rakumod` re-exports every `Deps::Funcs` routine this way to inject `:class(Deps)`.

mutsu's bareword-call dispatch resolved straight through its fast-path caches to the **registry**
entry for the bare name — the real, unwrapped routine `use Inner;` had just registered under the
flat `GLOBAL::` key — never checking whether `sub EXPORT`'s returned map had installed a *different*
callable under the same name in `env`. The wrapper's injected argument was silently dropped, and any
code relying on it (e.g. `Deps`'s `deps-root { ... }`, which needs `:class` to construct the right
object) failed downstream in a way that looked unrelated (`No such method 'parent' for invocant of
type 'Any'`).

A new interpreter-wide set, `export_amp_override_names`, is populated whenever `install_export_symbol`
installs an `&name` value into `env`. Unlike the existing `amp_param_shadowed_names` mechanism (a
`&`-sigil parameter shadow, gated by `free_var_syms` so it only applies to a value a closure actually
captured), an `EXPORT`-installed override is a lexical import of the whole importing compunit, so a
bareword call to that name is now re-checked against `env` regardless of `free_var_syms` — the
`env_callable_is_lexical_override` filter (already used for the parameter-shadow case) still decides
whether the `env` value is actually a different callable from the registered routine, so an
unmodified re-export continues to resolve exactly as before.

See #8746. A related but separate bug (a closure created inside `sub EXPORT`'s own `use`-containing
body loses that import once the body's dynamic scope exits, independent of any name collision) was
found while testing this fix and filed separately as #8751.
