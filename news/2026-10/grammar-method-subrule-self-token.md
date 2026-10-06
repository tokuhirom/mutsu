# A grammar method subrule can delegate to a token with `self.tok`

A grammar method reached as a subrule (`method foo { self.bar }`) is invoked on a grammar
instance that carries the in-progress `orig`/`pos`. A token called on that instance restarted on
the empty string and so always failed. It now continues from the instance's own position, and the
Match a method subrule returns is filed under the subrule's name (`$<foo>`), matching rakudo.
Fixes #11799.
