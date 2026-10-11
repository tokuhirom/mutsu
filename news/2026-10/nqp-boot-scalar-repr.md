# nqp: BOOTInt/BOOTNum/BOOTStr boxes and isint/isnum/isstr

`nqp::bootint`, `bootnum`, `bootstr` now return real type objects, and
`nqp::box_i/n/s` into them make raw P6int/P6num/P6str boxes that
`nqp::isint`, `isnum` and `isstr` recognise (a Raku `Int`, `Num`, `Str` or
native `str` answers 0, as in Rakudo). `unbox_*` read the box back and
`nqp::hllize` turns it into the high-level value. Six of the seventeen ops of
#11553 are done; collections and native references remain.
