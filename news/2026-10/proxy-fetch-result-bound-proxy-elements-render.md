# A Proxy whose FETCH returns an array of bound Proxies renders its values

`sub f is rw { my @b; @b[0] := some-proxy(); Proxy.new(FETCH => sub ($) { @b }, ...) }` followed by
`f().raku` / `.gist` printed `[Proxy]`; the method-call receiver path now resolves the Proxy elements of
what a Proxy receiver FETCHed before a renderer runs (CSS::Properties' `$css.padding.raku`). Fixes #14556.
