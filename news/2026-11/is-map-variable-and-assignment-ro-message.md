# `my %h is Map` refuses stores like `Map.new`, and `X::Assignment::RO` shows the gist

`my %h is Map = a => 1; %h<b> = 2` dies in rakudo with "Cannot add key 'b' to
an immutable Map" (or "Cannot change key 'a' in an immutable Map"), the same
wording as a store into `Map.new(...)`. mutsu marked the variable itself
read-only, so the element store hit the by-name readonly check first and said
"Cannot modify an immutable value". The variable no longer carries that mark:
the Map's own `declared_type` refuses element stores, and the whole-variable
check refuses reassignment, as it already did for a `:=`-bound Map.

`X::Assignment::RO.message` now follows rakudo: it names the value's type
(`List`, `Pair`, `Map`, not `Any`) and shows its `.gist`, cut to 20 characters
plus `...` when the gist is longer than 23. Reassigning a `%` variable bound to
a `Set`, `Bag` or `Map` printed the value's `.Str` (`(a)`, `(a\t1)`) instead
of its gist (`(Set(a))`, `(Map.new((a => 1)))`).
