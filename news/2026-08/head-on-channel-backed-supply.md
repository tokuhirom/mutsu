# `.head(N)` on a channel-backed live Supply no longer drops every emission

`Supply.interval(...).head(3).tap({...})` — and the same shape on `Proc::Async`
output or an async socket's channel-backed Supply — used to call the tap
callback zero times: `.head`'s dispatch only special-cased a *Supplier*-backed
source (`supplier_id`); anything else fell into the *materialized* branch,
which reads the Supply's (still empty, since it had not even been tapped yet)
`values` array, takes `count` of that, and hands the result to
`make_supply_from_values` — a fresh Supply carrying neither `supply_id` nor
`supplier_id`. Tapping that empty Supply fired `done` immediately with nothing
ever delivered.

Fixed by giving a channel-backed source its own third branch: a derived
Supply carrying a fresh `supply_id` and a `parent_supply_id` back to the
source (the same shape `.lines` already uses so its values keep arriving on
the *source*'s channel), plus a new `head_limit` attribute. The live act-loop
pump every channel-backed tap already runs through
(`Interpreter::run_supply_act_loop`) now takes an optional head-limit
parameter: once that many plain-value units have been dispatched, it fires
the tap's `done =>` handler itself and stops — the same outcome a real
upstream `Done` produces, needed here because an infinite source like
`Supply.interval` never sends one on its own.

`.head(*)`/`.head(Inf)` on such a source is unbounded (matches raku); `.head`
chained onto a `.lines`-derived Supply still splits into lines before
counting toward the limit, since the derived attrs carry `is_lines`/
`line_chomp`/`enc` forward. Scoped to a plain `.tap()`/`.act()`; `.head` on a
channel-backed source consumed via `whenever` inside a `react`/`supply` block
or a scheduled pump still goes through the same live-drain machinery but
without a head limit for now.

Pinned by `t/supply-head-channel-backed.t`.
