# An `is raw` / `is rw` block sub is a lexical binding for the closures nested in it

`plain_lexsub_name` excluded `is raw` and `is rw` subs, so a closure calling one went down the
by-name fallback ladder on every call (`function-fallback get-attr-obj=300` for 300 `has-foo` calls
in AttrX::Mooish). They now bind like any other block sub. The AttrX::Mooish single-thread loop
(`Foo.new`, `has-foo`, `.foo`, `clear-foo` x 1000) drops from 6.8 s to 4.5 s (raku 0.24 s); #17335
stays open for the remaining flat per-call cost.
