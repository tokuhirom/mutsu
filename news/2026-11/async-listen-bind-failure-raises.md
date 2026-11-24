# A failed `IO::Socket::Async.listen` bind no longer vanishes in a `react`

When `IO::Socket::Async.listen` could not bind its port (already in use, an
address family the host lacks) and the tap had no `quit` handler, which is the
case for a `whenever` in a `react`, mutsu dropped the error and carried on.
The `react` went on as if it were listening, so a test that kept a "ready"
promise after setting up the listener had its client fail with a bare
"connection refused" and no sign of the cause. This is the shape of
IO::Socket::Async::SSL's `t/upgrade.rakutest` failing on the CI runner
(#19058). The bind failure is now raised, as the refused-accept-thread case
already was, so the `react` dies with "address already in use" like rakudo's
does; a tap with a `quit` handler still receives it there. `upgrade.rakutest`
joins the battery whitelist.
