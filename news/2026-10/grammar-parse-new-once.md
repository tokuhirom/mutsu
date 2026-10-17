# A grammar's `new` runs once per parse

A grammar instance's `.parse`, and a user `parse` that defers to `Grammar`'s with `nextsame`, called a grammar's own
`method new` twice: once explicitly before the parse, and again for the start rule's `self.new`. Rakudo calls it once
per parse, and so does mutsu now (#13731).

The by-name `parse`/`subparse`/`parsefile` arm is gone too. A type object and an instance both reach `Grammar`'s rows
(ADR-11276 §9.46), which run one parse routine.
