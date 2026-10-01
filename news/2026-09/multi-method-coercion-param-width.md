# Multi-method dispatch ranks coercion and sigilled parameters like multi-sub

`multi method` ranking measured an `IO() $p` parameter by its target type (`IO`) instead of the
type it accepts (`Any`), and scored an unconstrained `%h` / `@a` parameter at a flat 1000 instead
of `Associative` / `Positional`. A Hash argument therefore picked `IO() $path` over `%data`.
Both now follow the multi-sub rules. Found via Config::Parser::toml, whose `t/01-read.t` now passes.
