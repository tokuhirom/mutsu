# mutsu

Raku compat interpreter

## Parse diagnostics

`mutsu` reports parser failures with structured metadata carried by `RuntimeError`:

- `code`: `PARSE_UNPARSED`, `PARSE_EXPECTED`, or `PARSE_GENERIC`
- `line`, `column`: 1-based source location when available

CLI output includes this metadata in a stable format:

- `Parse error metadata: code=..., kind=parse, line=..., column=...`
