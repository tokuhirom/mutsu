# `temp $x = Other.new` no longer rewrites the object it bound

Restoring a `temp`-ed scalar that holds an object wrote the saved attributes
into whatever instance the variable held at scope exit, assuming it was the
same object mutated in place. After `temp $x = Other.new` that instance is a
different object, so anything copied out of the scope (`$kept = $x`, a hash
element) was silently turned back into the old value. The write-back now
happens only when the live instance is the saved one; otherwise the variable
is simply rebound. This was one of the two causes of CSS::Stylesheet's nested
`@media` rules being filed under the wrong media query.
