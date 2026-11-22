# Smartmatch against a `Date`/`DateTime` subclass compares by value

`class T is Date { }; T.new(2020,1,1) ~~ T.new(2020,1,1)` was `False`. The
smartmatch only compared by value when both classes were exactly `Date` or
`DateTime`, so a subclass instance fell back to identity. A subclass now has
its parent's `ACCEPTS`, on every path (`~~`, `.ACCEPTS`, `when`, `grep`):

- A `Date` matcher accepts a `Date` of its own type on the same day, or a
  `DateTime` on that day.
- A `DateTime` matcher accepts any `DateTime` naming the same instant.

The last rule also fixes two DateTimes in different time zones, which were
compared field by field before (#18818).
