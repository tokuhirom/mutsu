RakuAST now preserves `native` package declarations as `RakuAST::Native`, including their
scope, parent type, `repr`, native traits, and body. Lowering restores the internal native marker
so existing NativeHOW handling applies `ctype`, `unsigned`, and `nativesize`. The round-trip
frontend ratchet grew by 18 files.
