# Match IO::Path::Parts single-item fallback methods

`IO::Path::Parts` now matches Rakudo's inherited single-item fallbacks:
`.elems` returns one, list and value methods contain the object itself, keys
contain zero, pairs and kv use the zero-to-self entry, and iteration visits the
object once. Positional indexing, flattening, and map coercion continue to
expose the three named path parts.
