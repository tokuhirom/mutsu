# Promise, Promise::Vow and Channel methods are method-table rows

`Promise` (`result`, `then`, `andthen`, `orelse`, `keep`, `break`, `vow`, `cause`, ...), `Promise::Vow` (`keep`,
`break`) and `Channel` (`send`, `receive`, `poll`, `close`, `fail`, `list`, `closed`, `Supply`, ...) no longer answer
through name-matching arms: each method is a row of the method table owned by the class Rakudo declares it on
(ADR-11276 §9.118). A vow's `keep`/`break` had two separate implementations; there is one now. `Channel.elems` dies
with Rakudo's message, `Cannot determine number of elements on a Channel`.
