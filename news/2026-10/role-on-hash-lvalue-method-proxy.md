# Method lvalue assignment through a Proxy on a role-mixed Hash

`self.AT-KEY($k) = v` inside a role applied to a Hash (`my %h is Hash::Ordered`)
died with "cannot assign through .AT-KEY on non-instance", because the invocant is a
`Mixin` rather than an `Instance`. The method is now called in lvalue mode and the
Proxy (or container) it returns is stored through. `Hash::Ordered`'s
`t/01-basic.rakutest` passes 18/18.
