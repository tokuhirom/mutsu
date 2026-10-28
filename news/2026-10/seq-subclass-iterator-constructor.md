# A subclass of `Seq` constructs from an Iterator and iterates

`unit class Gauge is Seq` died on `Gauge.new($iterator)` ("Default constructor
... only takes named arguments"), both directly and through `callwith`/`nextwith`
from the class's own `new`. An instance now carries the `Seq` that `Seq.new`
builds (`__mutsu_seq_value`), the way a `Range` subclass carries its range;
`.list`, `.head`, `.iterator`, `.map`, `for` and list assignment read it (#15265).
