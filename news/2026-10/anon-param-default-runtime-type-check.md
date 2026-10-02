# A wrong type for an anonymous parameter with a default fails at run time

`sub h(Int $ = 5) {...}; h("x")` used to report the compile-time-flavoured
`Calling h(Str) will never work with declared signature (Int $ = 5)`. Rakudo
does not reject a parameter with a default statically: the call fails at run
time with `Type check failed in binding to parameter '<anon>'; expected Int but
got Str ("x")`. `enhance_binding_error` now leaves a binding failure on a
defaulted positional parameter unwrapped, so mutsu reports the same message
(#11075). The bare `sub h(Int = 5)` spelling is a separate parse problem.
