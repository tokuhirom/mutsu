# CSS::Module: `|c` over a Hash, nested `also is`, `:i` proto calls, qualified rule actions

Gaps that kept `CSS::Module`'s own suite from running:

- A `|c` in a hash sub-signature (`-> % ( :$m!, |c)`) used to bind the whole
  hash as a positional; `Hash.Capture` is purely named, so `|c` is now a
  Capture of the named arguments no named sub-parameter took. `-> [$a, |c]` in
  a `for` header now parses too.
- `also is Calc` inside a class body names the `Calc` the body itself declares,
  not an outer type of the same name (the header-phase resolution fell back to
  the bare tail of the qualified name).
- A `rule` with `:i` calling a proto rule evaluated the candidate eagerly,
  computing every end of its body up front and firing the action of `||`
  branches the match never reached. The candidates are now re-scoped under
  `:i` once and entered as lazy frames.
- `<alias=.Pkg::rule>` dispatched the action under the package-qualified name,
  so the callee's action never ran; it now runs under the rule's own name.
- A `use` inside a method body is a BEGIN-time load for the whole unit, like one
  in a sub or block (`CSS::Module::CSS3::Fonts::AtFontFace.module` loads the
  actions classes the rest of the suite relies on).
- Multi dispatch: a destructuring sub-signature on a positional parameter makes
  a tie run the first candidate declared (rakudo trial-binds it) instead of
  raising "Ambiguous call", and the sub-signature of a slurpy is matched
  against the same flattened list the binder sees.
- A `:i` written directly against a quote (`:i'only'|'not'`) scopes over the
  whole alternation, not just its first branch.
