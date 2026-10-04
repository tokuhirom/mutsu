# ADR-0029's real `Test` sweep is complete

The prerequisite for ADR-0029 Slice 4, making the vendored upstream `Test`
module the default provider (#7554), has landed. The native provider has since
been removed (#7566). The sweep now exercises the upstream module on every run.

On `main` at `9840958a8`, the designated role-membership probe,
`roast/S02-literals/quoting-unicode.t`, passes 101/101 under the vendored `Test`.
Its six historical `X::Comp::FailGoal ~~ X::Comp` assertion losses are now
absent. The direct `t/exceptions/exception-role-membership.t` pin passes 27/27.
The full whitelisted roast suite passes 1426 files and 218756 assertions.

The bundled-library suite passes 311/326 files, with every whitelisted file
passing. The 15 remaining failures are outside the whitelist; none reports an
`X::` registration or role-membership mismatch. This is a current-state sweep,
not a controlled before/after experiment: the earlier 291/311 measurement used
a different corpus and many interpreter fixes have landed since ADR-0029.
Consequently, six recovered assertions in the designated probe are the
specific historical effect documented for this ADR; no broader suite-count
gain can be attributed to the exception hierarchy alone.
