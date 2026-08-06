# `roast/S03-operators/misc.t`'s `X::Syntax::DuplicatedPrefix` subtests already pass

`todo/tickets/duplicated-prefix-question-mark.md` tracked three related gaps in
`X::Syntax::DuplicatedPrefix` diagnosis, claiming two of them (`1%^^1` and
`555 ~~!~~ 666`, `roast/S03-operators/misc.t` tests 35/36) were the only real
losses keeping that file off some notion of "fully clean" under the real `Test`
module.

Re-checked while triaging the ticket backlog: both subtests already pass on
`main` (`ok 35 - %^^ fails to parse (RT #73198)`, `ok 36 - ~~!~~ fails to parse
(RT #76436)`), the file is whitelisted, and `prove` reports `Result: PASS` with
only the correctly-`# TODO`-marked test 38 failing. Whatever fixed the `^^`
metaop diagnosis (`news/2026-08/metaop-doubled-infix-base.md`) evidently covered
the `~~` case in this file's exact phrasing too, and the `%^^` lexing question
mentioned in the ticket turned out not to block this file either (`1%^^1` raises
`X::Syntax::DuplicatedPrefix` correctly through `EVAL`, which is what
`throws-like` exercises — direct top-level parsing of the bare expression still
diagnoses a plain `X::Syntax::Confused`, a narrower and separately-tracked
divergence, not this file's problem).

The third gap in the same ticket — `??` in term position should raise
`X::Syntax::DuplicatedPrefix` (and `Z??` should raise `X::Syntax::CannotMeta`
instead of falling through to term position) — was still real and unrelated to
any roast whitelist file. Kept open as
`todo/tickets/duplicated-prefix-question-mark.md`, trimmed to just that gap.
