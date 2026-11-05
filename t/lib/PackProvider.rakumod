# Like P5pack: the module provides `unpack` for Blob by installing the routine
# core's `Blob.unpack` looks for in its caller's lexical scope.
my sub unpack-method($buf, @template) { "{$buf.elems} bytes by {@template.join('|')}" }

BEGIN OUR::<EXPERIMENTAL-PACK> := &unpack-method;
