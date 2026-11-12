# A slang's grammar is composed once, not once per production

The slang host (ADR-15762) composes the user's grammar and actions roles into a
`Raku::Grammar` / `Raku::Actions` pair and caches them. The cache check asked
whether the composed grammar was `.defined` — but a grammar is a type object,
which never is, so every slang production the parser handed over (several per
declaration) recomposed both from scratch.

The cache is now keyed on the roles it was composed from. EEL's test files parse
4-5x faster: `t/31-assertion-integration.t` under `-c` went from 13.3 s to 2.7 s
(rakudo: 2.1 s), `t/34-per-event-selection.t` from 5.2 s to 1.7 s (1.5 s).
