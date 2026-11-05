# `X::f()` calls the `our sub` of a lexical class

A qualified call whose package is a lexical class (`my class X { our sub f { 7 } }; X::f()`) answered the string `X::f`: the sub is registered under the class's mangled storage name. The function resolution now translates the package through `resolve_lexical_type_key` before giving up.
