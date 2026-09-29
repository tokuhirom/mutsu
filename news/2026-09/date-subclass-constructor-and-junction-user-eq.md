# Date subclasses: qualified builtin calls, inherited `Date.new`, and Junctions through a user `infix:<eq>`

Working through `Date::WorkdayCalendar` (record was red, 0/4 files at parity) turned up three gaps:

- `$date.Date::succ` (a qualified call naming a builtin ancestor) died with "No such method". The
  qualified-instance dispatcher now runs the builtin on the receiver viewed as the qualifier class,
  so an overriding subclass method is not re-entered.
- `class Workdate is Date` with some `multi method new` candidates lost the inherited `Date.new`
  candidates: `Workdate.new($date)` raised "only takes named arguments". The constructor now falls back
  to the temporal ancestor's constructor and blesses the result as the subclass.
- With a user `multi infix:<eq>` in scope, `'a' eq any(...)` bound the Junction as a whole and collapsed
  to `False`. The infix-function path now autothreads a Junction one eigenstate at a time unless the
  candidate names a `Junction`/`Mu` parameter.

All four test files run at parity locally. The ledger record is not re-measured (no `raku` in this
container); run `ecosystem-sweep.yml` with scope=only after merge.
