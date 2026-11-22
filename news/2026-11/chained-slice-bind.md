# A `:=` into a chained subscript with a slice binds or throws X::Bind

`my @s = 1, 2; @s[0][0,1] := (7, 8)` died with `Cannot modify an immutable
Int (1)`. Rakudo raises `X::Bind` ("Cannot use bind operator with this
left-hand side"). The same bind on an Array element (`@s = [1, 2],`) silently
bound nothing, although rakudo binds it element by element (#18535).

The chained subscript store (`IndexAssignDeepNested`) now classifies a bind
that has a slice in its chain (`vm_chained_slice_bind.rs`):

- A slice at an inner level (`@s[0,1][0] := 7`) throws `X::Bind`.
- A last-level slice is bound key by key when the container the inner
  subscripts reach takes the bind: a real Array for a positional slice, a
  Hash for an associative one (`%h<a><x y> := (7, 8)`).
- Any other receiver throws `X::Bind`: an Int, a List, a Hash asked for a
  positional slice, or an Array asked for an associative one.

Each element binds its value, not a source variable's container, as in
rakudo. The expression form (`say (@s[0][0,1] := (7, 8))`) used to fail to
parse; it now parses as an ordinary indexed bind and takes the same path.
