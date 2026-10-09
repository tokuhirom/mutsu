# `.wrap` on a `proto method` no longer recurses

`G.^lookup('pick').wrap(...)` on an explicit `proto method` entered the dispatcher
wrap chain from the winning candidate, after the proto body's `{*}`, so the
wrapper's `callsame` re-ran the proto and re-entered the wrapper until the stack
overflowed. The chain is now entered where the proto body is intercepted, before
the body, and the candidate run by `{*}` bypasses it.
