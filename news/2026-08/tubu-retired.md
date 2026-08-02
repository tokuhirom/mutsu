# Retire the homegrown Tubu web framework

`t/lib/Tubu*` was a minimal, homegrown synchronous Sinatra/P6W-style web framework written in pure
Raku (routing, path/query/form params, cookies, before-hooks, JSON/HTML/redirect helpers, plus a
from-scratch HTTP request parser and response builder), driven by the integration test
`t/tubu-web-framework.t`. It was written in 2026-06, when no off-the-shelf Raku web framework ran
under mutsu, so that the batteries story had *some* web-framework answer and so that a realistic
blog could be demonstrated end to end.

That premise no longer holds. The web-framework slot's target is **Cro** (user decision 2026-07-31,
[docs/batteries/web-framework.md](../../docs/batteries/web-framework.md)): 61 dependents, a 28/28
raku baseline for `Cro::HTTP`, and the whole Air / Cromponent / Crolite ring behind it. That campaign
is moving — the `EXPORTHOW::DECLARE` MOP work retired the native `monitor` stopgap so OO::Monitors
now runs verbatim as a bundled battery — and it is the rung-2 answer BATTERIES.md asks for: grow the
interpreter until the real upstream module runs, rather than shipping a private dialect. A homegrown
framework maintained in the test-helper directory is exactly the parallel second implementation the
"1 operation = 1 implementation" rule exists to prevent, and it was strictly the weaker of the two.

So `t/lib/Tubu.rakumod`, `t/lib/Tubu/{Request,Response,Server}.rakumod` and
`t/tubu-web-framework.t` are deleted, and PLAN.md drops Tubu from the bundle candidate list and
removes the "promote the `t/lib` homegrown libraries" task entirely (its other half, `DBDishLite`,
was retired 2026-07-31 in favour of the bundled `DBIish`, so nothing is left to promote).

No interpreter coverage is lost: the two general bugs the framework originally surfaced already have
their own dedicated guarantee tests — `t/imported-sub-shadows-builtin.t` (an imported `get`/`post`
route declarator shadows the same-named core builtin) and `t/readonly-param-shadow.t` (a caller's
readonly parameter does not poison a callee's same-named `my` variable). The third finding, the
stored-Regex `<$var>` lexical capture loss, remains recorded in PLAN.md as an open item on its own
axis.
