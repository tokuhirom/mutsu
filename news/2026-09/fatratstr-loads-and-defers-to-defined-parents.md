# FatRatStr loads: allomorphs augment, `:D:` parents defer, `Int()` keeps big integers

Working the FatRatStr distribution took it from `blocked_load` to 3 of 4 test files at parity
(`t/03-makestr` is 36/37; the last assertion needs #10198).

- `augment class NumStr/IntStr/RatStr/ComplexStr/Allomorph` no longer raises "does not exist".
- A method call used to hand its deferral frame the type object instead of the receiver whenever
  the receiver had no attributes, so a parent `multi method g(A:D:)` failed its `:D` check and
  `nextsame`/`callsame` from a child answered Nil. The frame now carries the real receiver.
- `my Int() $x = "10938370151111111111"` and `Int()` parameters coerced a string past i64 to 0;
  they now keep the exact big integer.
