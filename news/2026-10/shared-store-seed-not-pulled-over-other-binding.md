# A seeded shared-store entry is no longer pulled over another binding

The cross-thread store is keyed by bare variable name, so a value it holds under
a name can belong to a different binding of that name. After an `await`,
`sync_shared_vars_to_env` pulled the entry of any name some thread had marked
dirty, including an entry that was only the spawning binding's snapshot (an
in-flight `my $response` placeholder). With the vendored Cro::HTTP, the
`my $response;` inside `ResponseParser`'s `supply { }` body was swapped for the
awaiting script's `Any`, so the second request of
`my $response = await $client.get(...)` in a loop returned `Any`.

Each store lineage now records which entries a thread actually wrote (`set`,
`with_entry_mut`); a name that is only a seed is skipped by the sync.
`t/concurrency/cro-client-repeated-await-response-lexical.t` pins the real
Cro::HTTP round trip. Re-keying the store by binding identity, the long-term fix,
stays open on #12204.
