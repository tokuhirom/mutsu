# `.raku`/`.gist`/`.Str` on a Proxy-subclass variable render the fetched value

`dispatch_proxy_subclass_method` answered `.raku`, `.gist` and `.Str` of a variable bound to an
instance of a `Proxy` subclass with a hard-coded `"ClassName()"`, ahead of the ordinary auto-FETCH.
rakudo renders the FETCHed value (`5`). The special case is gone, so the call falls through to the
FETCH like every other method. Found with the `annotations` distribution's `Binder`.
