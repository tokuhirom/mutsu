# `infix:<Z>(...)` now dispatches as zip

Calling the built-in zip operator by its fully qualified function name now
returns the same zipped sequence as ordinary infix `Z` syntax. The call no
longer falls through to expression reparsing and reports the unrelated
`Two terms in a row` error.
