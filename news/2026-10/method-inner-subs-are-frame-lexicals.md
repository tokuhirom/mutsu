# A method's call-only inner `my sub` is a frame lexical

ADR-0113 binds a call-only `my sub` of a `sub` body as a frame lexical: no
registry install, no registry snapshot, calls resolved at compile time. A
method body is compiled through a different path
(`compile_routine_closure_body`) that never ran the pass, so every call of a
method declaring a `my sub` still registered it twice and rolled it back.
`compile_method_closure_body` now runs the same pass for methods.

Callgrind, a method that declares and calls one `my sub` (Ir per call,
slope between 200 and 1200 calls): 89.3k -> 19.3k. `AttrX::Mooish`'s
`is mooish(:lazy)` loop (`Foo.new; .foo`, issue #15688), whose accessor
wrapper declares inner subs in its methods: 6.0M -> 4.4M Ir per iteration
(-27%), 4.2 ms -> 2.9 ms wall.
