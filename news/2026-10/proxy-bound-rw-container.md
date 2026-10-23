# A Proxy-bound variable or attribute is returned as its own rw container

An `is rw` routine whose tail names a variable bound to a `Proxy`
(`my $p := Proxy.new(...); sub f is rw { $p }`), a `$!attr` bound to one
(`$!a := Proxy.new(...)`), or an accessor alias (`^add_method('nick',
Foo.^method_table<a>)`) of such an attribute used to hand back a plain value, so
`f() = 7` silently dropped the write instead of running the Proxy's STORE. The
`$!attr` of a `Proxy` subclass (AttrX::Mooish's `AttrProxy`) is now promoted to
its shared container when passed to an `is raw` parameter, and
`Class.^mixin(R).^find_method` finds the accessors of R's attributes.
AttrX::Mooish `t/080-aliasing` (both variants) now passes next to `raku`.
