# A WhateverCode tested first by `and`/`or`/`||`/... is a compile-time worry

`@a.first((* > 1 and * < 5))` reads like one test, but a short-circuiting
logical infix (`and`, `or`, `xor`, `andthen`, `orelse`, `notandthen`, `&&`,
`||`, `^^`, `//`) does not curry: its first operand is a WhateverCode, a
closure, which is always true and defined. Rakudo says so at compile time,
and mutsu now does too:

```
Potential difficulties:
    WhateverCode tested first by and does not curry the and expression,
    and is always true and defined.
    Try a block using $_ instead if results are not as expected.
```

A bare `*` as the first operand is reported as `Whatever`, the operator is
named as written, and `no worries` silences it. The warning goes through the
same parse-warning channel as mutsu's other "Potential difficulties"; like
them it does not print rakudo's `------>` excerpt (#18139).
