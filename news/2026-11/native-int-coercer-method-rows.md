# Native integer coercions use their method-table rows

The remaining direct native-integer coercion branch has been removed from the
zero-argument method cascade. The existing `Cool` and `Int` rows now cover the
eleven coercions, including narrow-width methods such as `int8` and `uint16`,
for closed collection receivers and receivers without a settled dispatch shape.
The shared native coercion-name list controls admission, so that the dispatch
paths cannot disagree about which native type names are methods.

Ranges continue to coerce their element count, IO paths their string value,
and arrays and sequences their element count. Receivers outside `Cool`,
including `Pair` and ordinary user objects, do not acquire these methods.
Coercing the undefined `Int` type object now raises the same concrete-integer
error as Rakudo, while `Cool` keeps its zero conversion.
