# A thread's own cell is no longer replaced by another binding's snapshot

The cross-thread store is keyed by bare variable name, so a plain value it holds
under a name can belong to a different binding of that name. After an `await`,
`sync_shared_vars_to_env` pulled such a value over a binding the thread already
held as a cell. With the vendored Cro::HTTP, the `my $response;` inside
`ResponseParser`'s `supply { }` body was swapped for the awaiting script's
in-flight `my $response` placeholder (`Any`), so the second request of
`my $response = await $client.get(...)` in a loop returned `Any`.

A cell binding is now only refreshed by another cell; a plain foreign snapshot
is skipped. The regression test pins the real Cro::HTTP round trip
(`t/concurrency/cro-client-repeated-await-response-lexical.t`). Re-keying the
store by binding identity, the long-term fix, stays open on #12204.
