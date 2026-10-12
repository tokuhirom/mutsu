# RakuAST preserves supply statement source forms

The `supply STATEMENT` and `supply whenever ...` forms now retain their written
bodies across RakuAST conversion. They render as `StatementPrefix::Supply` and
lower through the same emitter expansion as `supply { ... }`. Round-trip tests
cover both structure and emitted values.
