# Backtrace frames know their package; a user `caller` sub wins

`Backtrace::Frame.code` now answers a `Sub` whose `.package` is the routine's
declaring package (it was always `GLOBAL`), a bareword call of a routine
declared later records its own call-site line instead of a stale pending one,
and a user-declared or imported `caller` routine replaces mutsu's native
`caller` extension (which is not a Raku core routine). Together these make the
`P5caller` distribution's `t/01-basic.rakutest` pass (12/12) under mutsu.
