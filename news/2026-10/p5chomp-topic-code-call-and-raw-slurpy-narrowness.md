# `.&chomp` writes through the topic; `()` beats `*@a is raw`

P5chomp exports Perl-style `chomp`/`chop` as multis. Two of their shapes did
not work under mutsu.

`$_ = "b\n"; .&chomp` calls `multi chomp(\s) { s .= chomp }`. A code-object
call (`.&g`) hands the binder its argument's source only by name. For every
other name the sigilless parameter then writes back to the caller's variable.
For `_`, though, the binder marked it readonly: a routine resets its own `$_`
before binding, so aliasing the name `_` would read the callee's topic rather
than the caller's. The call died with "Cannot modify an immutable value". Once
the parameter has a writeback queued to the caller's `$_`, it now stays
writable without the alias, and the value is copied back to the caller's topic
when the call returns.

`chomp()` with no arguments must reach `multi chomp() { die ... }`. Instead
it went to `multi chomp(*@a is raw)`, because the candidate ranking counted
the slurpy's `is raw` as "needs a writable argument", which made the slurpy
look narrower than the empty signature. Only a non-slurpy `is rw` / `is raw`
parameter counts now.

All three of P5chomp's test files pass. Writing through a sigilless
parameter bound to a `for` loop's topic is tracked as #11447.
