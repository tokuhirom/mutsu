# `CompUnit::DependencySpecification` is always an object

`CompUnit::DependencySpecification.new(:short-name<Foo>)` used to build a special
native value whose `.^name` was `Any`; only a depspec given a matcher was a real
object. Every depspec is now an instance of the class, and its methods are
method-table rows (ADR-11276 §9.115): `.Str` names the given matchers
(`Foo:ver<1.0+>:auth<zef:x>`), `.raku` renders the constructor call, and `.from`
is `Perl6`, as in Rakudo.
