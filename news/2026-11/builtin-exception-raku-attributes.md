# A built-in exception's `.raku` lists its attributes

Rakudo's `Mu.raku` lists every attribute `.new` builds, whether set or not, so
`X::NYI.new(feature => "x").raku` is
`X::NYI.new(feature => "x", did-you-mean => Any, workaround => Any)`. mutsu's
built-in exception classes declare no attributes, so their `.raku` was a bare
`X::NYI.new`, and so was a `Failure` wrapping one (#18645).

The attributes rakudo's 337 built-in exception classes declare are now a table,
`src/runtime/builtin_exception_attrs.tsv`, generated from rakudo by
`scripts/builtin-exception-attrs.raku`. The default `.raku`/`.gist` of a
built-in exception renders from it: own attributes first, unset ones as their
type object (`private => Bool`, `invocant => Mu`). Declaring the attributes on
the classes themselves (`.^attributes`) is the follow-up #18654.

The default `.raku` of any object now also lists a private attribute declared
`is built` (`class C { has $!x is built }; C.new(x => 1).raku` is
`C.new(x => 1)`), as Rakudo's does.
