# Loose boolean prefixes in listop arguments

An unparenthesized listop can now take an argument beginning with `so` or
`not`, such as `grep so *, @lines`. The parser keeps the following comma as
the separator between the listop's arguments. This lets the upstream
`HTTP::Message::Strict` module parse its chunked-content filter.
