# `Parameter.new`/`Signature.new`: constructing a signature at runtime

Locked and worked via the ecosystem distribution roulette (board:
[tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884)).
`Karabiner::CompModGenerator` 0.0.20 stays `red` (its only baseline file is
now blocked by a separate, newly-filed issue — see below), but the
investigation surfaced and fixed a real, previously entirely-unsupported
gap: `Parameter`/`Signature` had no constructor at all.

## `Parameter`/`Signature` are constructible types, not just introspection results

`Parameter` and `Signature` are ordinarily only materialized by the runtime
from a real declaration, but Raku exposes both as public, constructible
types too, precisely so code can synthesize a signature at runtime. The
small vendored zef module `Template::Classic` is a good example: its
`template(Signature $sig, Str $source)` reconstructs `$sig.perl` into real
declaration syntax and `EVAL`s a fresh `sub` from it, so a caller can build
a callable whose parameter names come from data rather than source code:

```raku
my @params = self.^attributes».name».substr(2).map: { Parameter.new(:name('$' ~ $_)) };
my &generate-rule := template Signature.new(:@params, :returns(Seq)), $template-text;
```

mutsu rejected this outright: `Unknown method value dispatch (fallback
disabled): new on Parameter`.

## Fix

`Parameter.new(...)` and `Signature.new(...)` are now real constructors,
added to the same `dispatch_new` match arm that already special-cases
`Pair`/`Rat`/`Complex`/etc. Two new functions in `src/value/signature.rs`
do the work:

- `sig_param_from_named_args` builds a `SigParam` from `Parameter.new`'s
  named arguments (`:name`, `:type`, `:optional`, `:named`, `:slurpy`),
  reusing the exact same `SigParam` → `Parameter` materialization
  (`sig_param_to_parameter_instance`) the runtime already uses for
  introspected parameters — so a constructed `Parameter`'s `.raku`, `.name`,
  `.type`, etc. all come from the same one code path as a real one.
- `sig_info_from_new_args` builds a `SigInfo` from `Signature.new`'s
  `:params`/`:returns`, recovering each `Parameter`'s original `SigParam`
  via a new `PARAM_REGISTRY` side table (mirroring the existing
  `SIG_REGISTRY` a `Signature` instance already used) when the `Parameter`
  came from `Parameter.new` itself, or deriving a minimal one from the
  `Parameter`'s own `.name`/`.type` attributes when it came from ordinary
  introspection instead (`Signature.new` must accept either).

Two details mattered for the rendered `.raku`/`.perl` text to actually
`EVAL` correctly and match rakudo:

- Every constructed `SigParam` needs `multi_invocant: true` — the parser
  already sets this for every ordinary declared parameter, but
  `SigParam::default()` gives `false`, which made `render_signature` insert
  a spurious leading `;;` (the `multi`-candidate boundary marker) before the
  first parameter.
- A `Parameter.new` with no `:type` renders as `Any $x`, not bare `$x` —
  unlike an actually-untyped *declaration*, which omits the type entirely.
  Likewise `Signature.new` with no `:returns` renders `--> Mu` explicitly.
  Both were verified against `raku` directly.

## Residue: a second, unrelated bug found along the way

Fixing this exposed a *different* interpreter gap the same file also hits:
a sub imported via `use` directly inside a class body is not callable from
that class's own method bodies (`unit class Template; use Template::Classic;
method rule_generator(...) { template ... }` throws `Unknown function:
template`, or in a more confusing shape, silently binds to a `List` instead
of calling the sub). This is unrelated to signature construction and was
filed separately as [#8883](https://github.com/tokuhirom/mutsu/issues/8883)
(`todo:deep`) rather than fixed here.

## Tests

`t/routines/signature/parameter-signature-new-construction.t` — `Parameter.new`
returns a working `Parameter`, `Signature.new` renders correct, EVAL-able
declaration syntax (verified end-to-end: EVAL a sub from a constructed
signature and call it), and `Signature.new` accepts introspected `Parameter`s
as well as freshly-constructed ones.
