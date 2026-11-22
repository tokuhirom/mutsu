# A Date/DateTime subclass's or mixin's own ACCEPTS is the matcher

`$topic ~~ $m`, where `$m` is a `Date`/`DateTime` subclass instance or a
`Date`/`DateTime` with a role mixed in, ran the built-in `Date.ACCEPTS` row and
compared by identity. The row was looked up by owner ahead of the instance's
own methods, so a user `ACCEPTS` never ran. It surfaced once every `but`
reblessed into a real `DateTime+{R}` type (#18695): Chronic's
`Chronic::Description.new ~~ ($now but DateTimeMatcher)` failed. The row now
steps aside when a user candidate of the method binds the call; a `multi` none
of whose candidates binds still leaves the row (#18817).
