# nqp-spawnprocasync test joins `out` chunks

`t/concurrency/nqp-spawnprocasync.t` asserted that a child's single `printf`
write arrived as exactly one `out` event. A pipe read is not tied to write
boundaries, so a reader scheduled mid-write could split it and the test failed
under `prove -j` (#13265). The assertion now concatenates the `out` payloads
before comparing.
