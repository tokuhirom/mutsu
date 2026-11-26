# The zef battery suite now runs the shipped v1.1.4 tests

`vendor/zef` has shipped zef v1.1.4 (`a5289115`) since September. The
`batteries.lock` row that pins which upstream test suite the battery gate
fetches still pointed at v1.1.3 (`0aa54f53`), so the release gate was testing
the v1.1.4 code against v1.1.3's tests (#19210, item 3).

The pin is now `a5289115`. Between the two tags, v1.1.4 adds a new
`t/repository-ecosystems.rakutest`, which covers an ecosystem index that
cannot be parsed and concurrent index updates, and extends
`t/utils-filesystem.rakutest` for the lock-file fix.

All 11 zef test files pass on mutsu, as they do on rakudo 2026.11. The new
`repository-ecosystems.rakutest` joins `batteries-whitelist.txt`.
