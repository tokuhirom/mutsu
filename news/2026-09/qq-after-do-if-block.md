# Code interpolation after a block-valued `do if`

The parser now preserves the statement boundary after a block-valued `do if`
when the construct is followed by a `qq[...]` expression on the next line.
This fixes the `Confused: Two terms in a row` load failure in
`Red::Driver::CommonSQL`, and closes another parser gap found while working
through [#7988](https://github.com/tokuhirom/mutsu/issues/7988).
