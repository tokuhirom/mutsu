# `.name` on a Hash or Array value reads its container descriptor

`.name` on a Hash or Array that is reached as a value now reports the
container's descriptor name. Before, only a call written directly on the
variable (`%h.name`) worked; every other path answered `Nil`. The fixed paths
are a `\m` parameter's `m.VAR.name`, `self` in a role mixed into a Hash, and a
Hash held in a scalar (`my $x = %h; $x.name`). The name is the declaring
variable (`%h`). An anonymous container (`Hash.new`, `[1]`) reports
`element`, as in rakudo. A List has no descriptor, so it still has no `.name`.

`@a.name` / `%h.name` written on a variable used to compile to a constant
holding the variable's own spelling. It now compiles to `.VAR.name`, the same
rewrite `.dynamic` already uses. So a `@`-parameter reports the caller's
container (`sub f(@x) { @x.name }; f(@b)` gives `@b`). `our`, `state` and
attribute containers keep their own names. Hash::Restricted's `nono` helper
no longer warns `Use of Nil in string context` (#11415).
