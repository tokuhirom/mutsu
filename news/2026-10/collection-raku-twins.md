# `raku` of a collection with user-rendered elements is the collections' twin

The by-name `raku` gate that rendered a collection holding an object with its own `raku` (or the
default `Class.new(...)` form) is gone (ADR-11276 §9.194): the renderer is the late interpreter
twin of `raku` on `List`, `Map`, `Seq`, `Pair`, `Capture` and the quant hashes, reached once the
pure `raku` rows decline, keeping an itemized receiver's `$`. Output is unchanged.
