# `temp Type.method = value` restores the old value

`temp Bla.^table = "temp_bla"` (Red's `t/23-metamodel-model.rakutest`) died with "Cannot modify an
immutable Str" and, once that was out of the way, never restored the value: `temp` could only
snapshot a variable invocant. A type-object invocant (`temp Type.method = v`, `temp Type.^meta = v`)
now lowers to `my $saved = Type.method; Type.method = v; LEAVE Type.method = $saved`, so the old value
comes back when the enclosing scope exits.
