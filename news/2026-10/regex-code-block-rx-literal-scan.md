# A code block holding `rx/.*?/` no longer hides the end of a regex declaration

The scan for the end of a `token`/`regex` code block `{ ... }` read the `/` after `rx` or `m` as a
division, so for `{ make rx/.*?/ }` the closing `/` (after an operator character) looked like the
opener of a regex literal and the block never closed. The declaration lost its source tree and the
RakuAST round trip refused it ("regex declaration without a source tree"). IO::Glob's
`token match:sym<%> { <sym> { make rx/.*?/ } }` was the real case (Archive::SimpleZip's dependency).
`rx/…/`, `m/…/`, `ms/…/` and `mm/…/` written without a space now open a nested regex.
