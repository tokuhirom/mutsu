# `lc .method` parses as a call on the topic method

`ord`, `chr`, `lc`, `uc` and `abs` followed by whitespace and a `.method` term
(`lc .contains('xn--') ?? a !! b`) were rejected as a bare use of the routine.
Rakudo takes the topic method call as the argument; mutsu now does too, while
the tight `ord.Cool` form stays an `X::Obsolete`. Found via the PublicSuffix
ecosystem distribution, whose module failed to load because of this.
