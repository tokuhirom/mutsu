# Promise and Channel hand out real Awaitable::Handle objects

`Promise.get-await-handle` and `Channel.get-await-handle` used to return the native
Promise/Channel itself, so `$p.get-await-handle ~~ Awaitable::Handle` was false and
`.already` / `.result` / `.subscribe-awaiter` did not exist. They now return handles that do the
core role (ADR-15519 unit 3): a settled promise or a channel with a queued value gives an
already-settled handle, a closed empty channel an already-failed one
(`X::Channel::ReceiveOnClosed`), and a pending source a handle whose `subscribe-awaiter` fires with
`(success, result-or-cause)` once the source settles. `await` on native promises and channels is
unchanged.
