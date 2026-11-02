# A container attribute keeps its `is default(...)` after assignment

`has %!dflt is default(7)` answered `7` for a missing key until the attribute was assigned
(`%!dflt = %x`); afterwards it answered `Any`, because the assigned value is a fresh container and
nothing carried the declared element default onto it. The by-name attribute store now re-applies the
default, looked up through the class MRO so an inherited attribute keeps its declaring class's
default. `@!a is default(...)` is covered the same way (#15547).
