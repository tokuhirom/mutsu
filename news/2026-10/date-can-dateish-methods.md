# `Date.can` / `DateTime.can` see the Dateish methods

`$date.can('week-number')`, `weekday-of-month`, `days-in-month`, `is-leap-year`, `earlier`, `later`,
`truncated-to` (and `posix`, `utc`, `whole-second`, `in-timezone`, ... on `DateTime`) answered False
although the methods worked, because the native method row catalog lacked them. Adding the rows fixes
`Date::Calendar::Strftime`'s `%V` specifier (gated on `.can('week-number')`), so its `t/08-sub.rakutest`
now passes.
