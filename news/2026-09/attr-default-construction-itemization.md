# A `$`-sigil attribute's declared default is now itemized at construction

A `$`-sigil attribute is a Scalar container, so an Array/Hash/Seq/Slip value
it holds should render with the itemizing `$` prefix and stop flattening
under the single-argument rule -- exactly what `my $x = [1, 2, 3]` produces,
and exactly what the accessor store already did since #9023. That fix only
covered `assign_method_lvalue_with_values`, so an attribute that still held
its *declared default* (never written through the accessor) read back
un-itemized: `class D { has $.w is rw = {a => 1} }; D.new.w.raku` gave
`{:a(1)}` where rakudo gives `${:a(1)}`.

The construction-time paths that evaluate a `has $.x = <default>` initializer
and seed the attribute cell directly are not reachable from
`assign_method_lvalue_with_values`, and there turned out to be four of them:
the native no-BUILD `.new` fast path (`build_native_default_instance`), the
interpreter's pre-BUILD attribute fill in `dispatch_new` (both its common
loop and its qualified-per-class-attribute branch for diamond-inheritance
name collisions), the post-BUILD deferred-initializer pass
(`apply_post_build_attr_defaults`), and `.bless`'s own default-fill loop.
Each now applies `itemize_attr_store_value` to the evaluated default before
committing it, matching the accessor store. `@`/`%` defaults are unaffected,
since that helper already no-ops for any sigil but `$`.

Pinned by `t/oo/attribute/attr-default-construction-itemizes.t`, covering all
four construction paths plus the `@`/`%`-stays-un-itemized and
plain-scalar-unaffected cases.

Closes #9040.
