# Date/DateTime formatters render live and survive conversions

A `Date`/`DateTime` `:formatter` used to be run once, at construction, and its
output cached in a hidden `__formatter_rendered` attribute that the pure
stringifier read back. Any value derived from the original either lost the
formatter (`.utc`, `.in-timezone`, `.local`, `.later`, `.earlier`,
`.truncated-to`, `Date.succ`) or, worse, copied the stale cached string along
with it (`$dt.clone(:hour(3))` and `$date + 3` printed the *original* date). `DateTime::Format`'s RFC 2822 test failed on exactly
this: `~$dt.utc` printed ISO 8601 instead of the formatter's output.

The cache is gone. The formatter is only attached to the value, and every
string context — `.Str`, `.gist`, `.Stringy`, prefix `~`, interpolation and
infix `~` — calls it against the value being stringified. The conversions now
carry the formatter over the way Rakudo does (`later`/`earlier`/
`truncated-to`/`in-timezone`/`utc`/`local`, `Date.new($date-or-datetime)`,
`.Date`/`.DateTime` on their own type), while `DateTime + Duration` drops it,
also as in Rakudo. `.utc` on a `DateTime` subclass now keeps the subclass.
