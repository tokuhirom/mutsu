# An attribute typed with a `my class` reports that class from `.type`

A lexical class (`my class P`) is registered under a scope-specific storage
name, but an attribute typed with it (`has P $.pos`) recorded only the short
spelling, so `$attr.type` was a fresh `P` package: not `=== P`, and with no
attributes of its own. JSON::Unmarshal decides how to build a nested object
from `$attr.type.^attributes`, so a `my class TestClassPos does Positional`
attribute fell into its "type mismatch" candidate. The attribute's type is
now resolved to the lexical class while it is in scope (and on introspection
as a fallback), so `.type` is the class itself even after the declaring sub
has returned. All 11 of JSON::Unmarshal's test files pass.
