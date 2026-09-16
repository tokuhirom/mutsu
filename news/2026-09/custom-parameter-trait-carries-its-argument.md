# A custom parameter trait's argument reaches dispatch

`sub MAIN(Bool :$timer is option<!>) { }` — App::Prove6's actual signature,
via Getopt::Long — died with `Can't use unknown trait 'is' -> 'option' in a
parameter declaration`, even though Getopt::Long exports exactly the
`trait_mod:<is>` candidates that accept `option`:

```raku
multi sub trait_mod:<is>(Parameter $param, Argument :option($argument)!) is export { ... }
multi sub trait_mod:<is>(Parameter $param where $param.named, Str:D :getopt(:$option)!) is export { ... }
```

`check_param_custom_traits` (the VM-side check that dispatches a custom
parameter trait, added for Cro::HTTP::Router's `is query` et al.) always
handed every candidate a hardcoded `True` as the trait's named argument —
the parser only ever recorded the trait *name*, discarding whatever argument
followed it. Neither of Getopt::Long's candidates accepts a bare `Bool`, so
dispatch always failed and the fallback "unknown trait" error fired, masking
the fact that a real candidate was right there.

Two changes fix this:

- The parser now *parses* a trait's optional argument — the `<!>` word-quote
  form and the `('utf8')` parenthesized form — into a real `Expr`, instead of
  merely skipping past it. `ParamDef` gained a sparse `trait_args: Vec<(String,
  Expr)>` field (most traits carry no argument and still dispatch with
  `True`, unchanged) threaded through every parameter-parsing site: ordinary
  signatures, sub-signatures, pointy blocks, and `for`/`with`/`while` loop
  parameters. `check_param_custom_traits` evaluates the matching entry and
  passes the real value.
- `check_param_custom_traits` now discriminates "no candidate accepted this
  trait" (raku's `X::Comp::Trait::Unknown`) from a real error a *matched*
  candidate's own body raised, the same way the sub-level custom-trait
  dispatch already does. Previously any error from inside a trait handler's
  body was collapsed into the generic "unknown trait" message, which would
  have made a genuine bug in a trait's own code indistinguishable from a
  typo'd trait name.

Pinned by `t/oo/trait/custom-parameter-trait-argument-dispatch.t` (a fixture
mirroring Getopt::Long's own two-candidate-by-argument-type shape, without
depending on Getopt::Long itself), which passes under `raku` too.

This clears the specific "unknown trait" symptom in
[#8560](https://github.com/tokuhirom/mutsu/issues/8560). App::Prove6 still
doesn't load end to end: Getopt::Long's own `trait_mod:<is>` bodies hit two
separate, pre-existing bugs once dispatch actually reaches them — a dynamic
`does Role(:named-arg(...))` loses the attribute value
([#8577](https://github.com/tokuhirom/mutsu/issues/8577)), and a dynamic
`does` cannot resolve a lexically-scoped, dot-qualified role (`my role
Formatted::Named`) declared earlier in the same file
([#8578](https://github.com/tokuhirom/mutsu/issues/8578)).
