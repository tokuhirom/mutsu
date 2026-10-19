# A user subclass of `Range` is constructible and acts as its range

`class Q is Range {}; Q.new(1, 5)` died with "Default constructor for 'Q'
only takes named arguments": the subclass never reached `Range.new`. It now
carries the range `Range.new($min, $max, :excludes-min, :excludes-max)`
builds as a native payload, the convention `Pair`, `Int` and `Str`
subclasses already use, so `Range`'s methods answer on it, `.raku`/`.gist`
render the range and `.Str` its elements (#14015).

Smartmatch against an instance of such a subclass (`3 ~~ $q`, and likewise
`3 ~~ MyInt.new(3)` or `'a' ~~ MyStr.new(value => 'a')`) now accepts through
the payload's `ACCEPTS`, which the subclass inherits; it compared the
instance itself before.
