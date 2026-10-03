# `is equiv(&infix:<&&>)` and `is equiv(&infix:<||>)` get their real precedence

A user operator declared `is equiv(&infix:<&&>)` or `is equiv(&infix:<||>)` used to fall back to the
additive level, because the parser had no precedence level for `&&`, `||`, `^^` and `//`. Mixed
chains such as `a «&» b «||» c «&» d` therefore grouped left to right. The logical operators now
have their own levels, and custom operators at those levels are parsed by the `&&` and `||` layers.
Found through the FunctionalParsers suite, whose `t/12-alternatives-first-match` now passes.
