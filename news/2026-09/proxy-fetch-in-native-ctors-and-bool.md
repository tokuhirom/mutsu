# Proxy results are FETCHed by native constructors and boolean context

A `Proxy` returned by an `is rw` `AT-KEY` (as `XML::Element` does) was passed
unfetched to the native `Date`, `DateTime` and `IO::Path` constructors, and
`not`, `?` and `so` treated any bare `Proxy` as true. Both now FETCH first, as
rakudo's decontainerization does. Found by working the `Printing::Jdf`
distribution's suite.
