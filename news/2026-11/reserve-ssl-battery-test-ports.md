# Battery gate: reserve IO::Socket::Async::SSL's fixed test ports on CI

The `release.yml` smoke run on main failed the whitelisted battery file
`IO::Socket::Async::SSL/upgrade.rakutest` once, with
`# address already in use (address: localhost:54328)`. The cause was the CI
environment, not mutsu (#19193).

The upstream suite listens on hard-coded ports 54328-54340. Those ports lie
inside Linux's ephemeral range (32768-60999). The gate runs the dist's files
serially, and `thread-stress.rakutest` runs just before `upgrade.rakutest`.
It leaves 80-200 localhost client sockets in TIME_WAIT on ephemeral ports.
`connect()` prefers even ports, so each run has roughly a 1% chance that one of
those sockets holds 54328. A client socket does not set `SO_REUSEADDR`, so the
listener's bind then fails with EADDRINUSE. Batteries that run in parallel
connect to localhost as well, which adds to the chance.

Measured in a container: when 54328 is held by an outgoing connection, either
ESTABLISHED or client-side TIME_WAIT, `raku` and `mutsu` fail the file in the
same way and print the same message. Server-side TIME_WAIT, which a previous
run of the file leaves behind, does not break a rerun under either one, because
both set `SO_REUSEADDR` on the listener.

A quarantine entry would not have absorbed the failure. The gate retries
immediately, well inside the 60s TIME_WAIT, so all three attempts would collide.
Instead, both workflows that run `scripts/battery-testsuite.sh` (`ci.yml`'s
`test-suites` job and `release.yml`'s `batteries` job) now add 54328-54340 to
`net.ipv4.ip_local_reserved_ports` first. This only stops the kernel from
choosing those ports for outgoing connections; an explicit bind still works,
and the gate is not weakened. With the range narrowed for a check, an
unreserved run handed out 54328 and a reserved run never did.
