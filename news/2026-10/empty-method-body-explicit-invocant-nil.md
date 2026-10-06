# An empty method body with a typed explicit invocant returns Nil

`method m(D:D:) {}` answered `Any` where rakudo (and an implicit-invocant method) answers `Nil`.
A constrained invocant takes the full method-call path, whose fall-through read the body's value
from `$_`, which is always `Any` on method entry. It now returns `Nil` like the fast path (#11861).
