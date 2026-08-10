# Cro::HTTP::Router "when X::Cro::BodyParserSelector::NoneApplicable needs parens" parse failure no longer reproduces

The ticket
`deeply-qualified-exception-class-in-when-clause-not-recognized-as-type-in-cro-router`
reported that loading the real, vendored `Cro::HTTP::Router` failed to
parse: the `when X::Cro::BodyParserSelector::NoneApplicable { ... }` clause
inside its CATCH block tripped the undeclared-`X::`-bareword block-gobble
heuristic (`parser/stmt/control/given_when.rs`), because the type —
declared in the transitively-`use`d `Cro::BodyParserSelector` — was not in
the parser's user-type registry at that point.

Re-checked 2026-08-11 with the full Cro `-I` set
(`tmp/cro-work/inc-paths.txt`) and a cleared precomp cache:
`mutsu <full -I list> -e 'use Cro::HTTP::Router;'` now succeeds, and
`t/http-router-plugin.rakutest` (whose load-failure this was) runs 7
subtests with a single unrelated failure. One of the general
module-scan/type-registration fixes that landed since the ticket was filed
(2026-08-10) resolved it; no single PR was identified as the fix.

Two clarifications recorded for the future:

- The ticket's "minimal deterministic repro" used a SINGLE `-I` (the
  Cro::HTTP checkout's lib only). In that configuration
  `Cro::BodyParserSelector` genuinely cannot be found (it lives in the
  Cro::Core checkout), so its types are legitimately unregistered and the
  gobble heuristic fires — that is a missing-dependency situation, not the
  bug. The comparison "raku succeeds with the same single `-I`" was
  misleading: the system raku has Cro::Core *installed*, so its `use`
  resolved through the installed repo. (A nicer diagnostic for the
  cannot-resolve-`use` case — failing at the `use` itself instead of at a
  downstream `when` — would still be an improvement, but it is a UX issue.)
- The remaining `http-router-plugin.rakutest` failure ("Local
  configuration in included route handler not affected by outer") is an
  unrelated attribute-corruption bug, diagnosed the same day:
  `todo/tickets/reconcile-attrs-adopts-caller-frames-attr-cell-from-env-chain.md`.
