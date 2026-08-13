# Value-position `BEGIN` resolves prior constants

An arithmetic expression inside a value-position `BEGIN` now resolves a
sigilless constant declared earlier in the same scope. Previously, phaser
reordering extracted the `BEGIN` expression and compiled it before the
constant declaration, so the constant name became a bareword string and
numeric coercion failed.

The phaser lifter now limits expression extraction to `CHECK` and `INIT`.
Value-position `BEGIN` remains on its dedicated `BeginOnceExpr` compiler path,
which preserves source-order constant visibility and still memoizes the result
at most once.

Regression test: `t/constant-begin-initializer.t`.
