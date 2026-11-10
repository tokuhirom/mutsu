# `$obj<k> = v` through a `handles <AT-KEY>` forwarder

When a class delegates `AT-KEY` (`has $.store handles <AT-KEY EXISTS-KEY>`) to an object whose
`AT-KEY` returns an `is raw` Proxy, the subscript store `$obj<k> = v` was dropped: only an
`AT-KEY` declared `is rw`/`is raw` on the class itself was routed through the method-lvalue
machinery, not a forwarder. The forwarder now counts too. Found by Qwiratry's `Table`, whose
fields are registered this way (`memory-taop`, `taop-acceptance-4.7-5.4` now match raku).
