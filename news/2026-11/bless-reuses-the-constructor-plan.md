# `.bless` reuses the constructor plan's seed defaults and attribute list

`.bless` was about three times as expensive as `.new` on the same class. A class
whose attributes are typed but have no initializer (`has Str $.scheme;`, the shape of
`Cro::Uri` and most Cro message classes) gets a parser-synthesized "seed" default
that evaluates to the declared type object. `.new`'s native constructor evaluates
that default once per class and keeps it in the cached per-class constructor plan
(`NativeCtorPlan::seed_defaults`). `.bless` ignored that memo, so it ran the default
expression through the interpreter for every typed attribute on every call. Its
`is required` check also re-collected the class's attribute list from the MRO on
every call, although the same plan already holds it.

`.bless` now reads and fills the same seed-default memo, and the required-attribute
check reads the plan's attribute list. On a class with nine typed role attributes and
a `TWEAK`, `U.bless(|%parts)` went from 124k to 58.9k instructions per call
(`.new` on the same class: 37.5k). On a release build that is about 22 µs to 12 µs
per bless. `Cro::Uri.parse-ref` blesses once per parsed reference.
