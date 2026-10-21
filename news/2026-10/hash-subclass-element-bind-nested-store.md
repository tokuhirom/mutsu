# An `is Hash` / `is Array` instance takes element binds and nested stores

Before this change, element binds and nested stores into an instance of a
class that `is Hash` or `is Array` behaved differently from the same operations
on a plain Hash or Array. Three cases:

- `$d<k> := $x` copied the value instead of binding the element to `$x`'s
  container, so a later write to either side did not reach the other.
- A nested store into such an instance held by another container replaced the
  instance with a fresh Hash. `@a[0]<k> = v` and `@a[0]<k> := $x` both did
  this.
- `%h<k><z> = v` dropped the write.

All three now go into the instance's backing storage, as they do for a plain
Hash or Array, and the instance keeps its class.

PDF's `PDF::COS::Dict` and `PDF::COS::Array` are such classes, and its object
graphs nest them (#12907).
