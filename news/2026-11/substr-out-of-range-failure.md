# `substr` out of range fails as rakudo does

`"abc".substr(10)` returns a Failure of `X::OutOfRange`. Thrown, it reads:

```
Start argument to substr out of range. Is: 10, should be in 0..3; use *-10 if you want to index relative to the end
```

mutsu's Failure carried the literal text `X::OutOfRange` as its message and had
no `.comment`. A negative Int start (`"abc".substr(-1)`) counted from the end
and answered `"c"`, and a negative length was clamped to zero. rakudo fails
both: only a WhateverCode (`*-1`) indexes from the end, and a negative length
is "Number of characters argument to substr out of range". The message is now
built from the exception's attributes, and both cases fail as in rakudo.
