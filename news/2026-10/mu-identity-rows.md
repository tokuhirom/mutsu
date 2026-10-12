# `Mu`'s identity and definedness methods are method-table rows

`self`, `defined`, `DEFINITE`, `Bool`, `WHICH`, `WHERE` and `clone` are now rows of `Mu` in the built-in method table
(ADR-11276 §9.91), reached by every receiver: each shape, every type object, a `Junction`, a code object and an instance of a
user class. The seven name-matching arms of `methods_0arg/dispatch_core_coerce.rs` that used to answer them are gone, and the
pure arity cascades drop from 174 to 167 quoted-name arms.

Three answers moved to Rakudo's on the way: `Match.self` is the match object (it was the matched string), and `Capture.clone`
and `Uni.clone` answer instead of throwing `X::Method::NotFound`. `Mu.clone` of an instance and the interpreter's clone with
`:attr(v)` twiddles now start from the same attribute copy.
