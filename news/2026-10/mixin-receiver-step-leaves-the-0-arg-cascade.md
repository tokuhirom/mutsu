# A `but`-mixed built-in value's overrides leave the 0-argument cascade

The `ValueView::Mixin` block of the 0-argument cascade is now
`receiver_step::mixin_step`: a plain allomorph's row first, then the mixin's
overrides (`Bool`, `Str`, the stringifying methods, a mixin key named after the
method, `clone`, `flat`, scalar `.list`), then the wrapped value's own method.
The cascade acts on the step like it acts on the other receiver steps. The
order is pinned in `t/oo/mixin-builtin-receiver-overrides.t` (#14327).
