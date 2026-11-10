# A generated attribute accessor called with a positional argument dies

`$o.attr(2)` on a `has $.attr is rw` accessor silently assigned 2 (the rw-setter convention used
by `$o.attr = 2` leaked into plain calls). It now dies with rakudo's "Too many positionals passed"
error; assignment goes through a dedicated path. Found with Template::Nest::XS `t/00-basic`.
