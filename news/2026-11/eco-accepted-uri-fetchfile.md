# Ecosystem ledger: URI::FetchFile's HTTP::UserAgent test is an accepted divergence

`URI::FetchFile`'s `t/060-http-useragent.t` exercises its HTTP::UserAgent
provider only when that module can be `require`d. The oracle rakudo has no
HTTP::UserAgent, so it skips 6 of the 8 tests. mutsu bundles it, so the
provider really fetches `http://example.com`, which the sweep's network-less
sandbox cannot reach. It is the Dev::ContainerizedService / SOAP::Client
situation (#15525), recorded in `ecosystem/accepted-divergences.toml` on the
same decision. The distribution re-grades green.
