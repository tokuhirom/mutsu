# `self.R::output` on a mixin reads the class attribute instead of recursing

`self.R::output`, where `output` is the public accessor of the attribute
`has $.output` of the class `R` and `self` is an `R` object with a role mixed in
(`$r but role { method output { ... self.R::output } }`), re-entered the mixin's
own `output` until "Too deep recursion": the qualified-call path for a mixin
resolved only methods the class declared, not its generated accessors, and fell
back to the ordinary dispatch that picks the mixin's override. It now reads the
inner instance's attribute, as the plain-instance path already did. Found while
reducing Jupyter::Kernel `t/06-magic.rakutest` (#16584).
