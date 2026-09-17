# An attribute type constraint no longer resolves to its own compound-named class

`has Supply $.Supply;` inside a class declared with a compound name whose
last segment matches the constraint (`class Lumberjack::Dispatcher::Supply
{ has Supply $.Supply; ... }`) resolved the type `Supply` to the
*enclosing class itself* instead of the core `Supply` type. Assigning a real
`Supply` object to the attribute then died with a bogus type-check error
(`expected Lumberjack::Dispatcher::Supply but got Supply (Supply)`).

The bug lived in `resolve_type_name_for_owner` (`src/runtime/types/
type_registry.rs`), which resolves an attribute's type-constraint name
against its owner class's name chain. Its owner-chain walk treated every
`::`-split prefix of a *compound declared name* (`class Foo::Bar::Supply`
declared at file scope, not nested inside a real `Foo::Bar` package) as a
genuine lexical scope, so stripping the class's own last segment and
re-appending the attribute's type name reconstructed the class's own full
name and matched it. This is the same footgun `resolve_type_in_current_package`
was already guarded against (`compound_name_segment_is_not_a_scope`,
originally fixed for a bareword type reference in a method body, e.g.
`class Foo::List { method m() { List.new } }` resolving `List` to itself);
`resolve_type_name_for_owner` just didn't share the guard.

Found working the `Lumberjack::Application` ecosystem distribution's test
suite — `Lumberjack::Dispatcher::Supply` (`has Supply $.Supply;`) is exactly
this shape. Fixing it brought `t/050-proxy.t`'s sibling `t/040-supply.t` from
0/5 to a clean 5/5 pass, matching rakudo.

Regression test: `t/vm/scope/compound-declared-name-is-not-a-scope.t`
(new case appended to the existing suite for this exact class of bug).

Two residual findings from the same distribution were filed as issues rather
than fixed here: #8672 (`HTTP::Server::Tiny` mis-parses a POST body as
chunked, dropping `Content-Length`) and #8673 (the vendored `JSON::Fast` is
far too slow parsing a real-world ~300KB document — `todo:perf`).
