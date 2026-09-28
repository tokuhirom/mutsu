# `is Num` subclasses carry their value; `"-5.9".abs` is a Rat

Two numeric gaps found by the Math::FractionalPart distribution, drawn at
random by the ecosystem roulette.

**A user subclass of `Num` had no value.** `class F is Num {}; F.new(2.5)`
built an instance with no payload, so it stringified as `F.new`, added as `0`,
and `.Real` died with "No such method 'Real' for invocant of type 'Any'".
Subclasses of `Int` already kept their integer in a reserved attribute that
arithmetic, stringification, `.gist` and the native method layer all read. A
`Num` subclass now keeps its float the same way (`Num.new($x)` boxes `$x.Num`,
so a `'-3/2'` argument is `-1.5`). The helper that used to read only the `Int`
payload is now `builtins::numeric_subclass` and reads either payload, so every
place that understood an `is Int` instance understands an `is Num` one too.

**A Cool numeric method parsed a decimal string as a Num.** `"-5.9".abs` was
the Num 5.9000000000000004 rather than the Rat 5.9, because the method form
tried an `f64` parse before Raku's own numifier. That made
`"-5.9".abs - "-5.9".abs.floor` come out as `0.9000000000000004`. The method
form now numifies a Str the same way `.Numeric` and the `abs(...)` function
form do.

Math::FractionalPart goes from 2 to 6 of its 6 test files at parity.
