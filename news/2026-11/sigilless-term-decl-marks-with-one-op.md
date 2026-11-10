# `my \k = EXPR` marks its readonly term with one op instead of two general stores

A readonly sigilless term declaration cleared and re-set its
`__mutsu_sigilless_readonly::\k` marker with two `SetGlobal` stores, each running the general
string-keyed store path (~24k instructions per declaration, `for ^20000 { my \k = 5 }` at 5.0 us
against 0.3 us for `my $k = 5`). The compiler now emits `SetTermMark`, which inserts the
compile-time key directly, the way `MarkSigillessBind` already did. The same loop is 0.6 us.
Part of #17376 (Monad `t/14_concurrent_types.t`, #17331).
