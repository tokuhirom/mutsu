# Iterate a bound Iterable in array assignment

A scalar bound with `:=` to a user-defined `Iterable` now passes that value to
array assignment without itemizing it. The array consumes the object's
iterator, while an ordinary scalar assignment still keeps the object as one
item.
