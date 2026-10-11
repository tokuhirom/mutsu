# `my int $x = my str $ = ...` coerces like rakudo

A native `str` declared inline and stored into a native `int` (`my int $h = my str $ = $line.substr(1,2)`, the "fast Str to int conversion" idiom) now takes `nqp::coerce_si` instead of raising "Cannot convert string to native integer type". IRC::Log::Colabti's two test files load and run under mutsu as a result; `t/02-search` is at parity.
