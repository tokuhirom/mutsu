# `OUR::c` reaches a file-scope constant

`constant c = 1; say OUR::c` died with "Could not find symbol '&c' in 'OUR'": the
qualified bareword was read as a sub call. At file scope `OUR::name` now resolves the
unit's sigil-less constant through its term binding, as `GLOBAL::name` already did (#13198).
