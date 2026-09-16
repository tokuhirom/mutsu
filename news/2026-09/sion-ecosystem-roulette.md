# SION ecosystem parity investigation

The random ecosystem-parity draw selected SION 0.1.0. Rakudo passes all four
distribution test files; mutsu passes the load test and remains partial at one
of four baseline files (67 of 150 assertions).

The three remaining regression files reduce to four independent interpreter
findings: dynamic regex `:pos` expressions are discarded (#8515), a `buf8`
argument is lost during proto-method redispatch (#8516), `NFC` values do not
bind to typed `Int` callback parameters (#8517), and `%a` formatting does not
normalize subnormal floats (#8518). Each ticket includes a standalone
Rakudo-versus-mutsu repro.
