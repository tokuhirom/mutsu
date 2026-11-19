# `$*CU`: a multi-part setting type name resolves to a ResolvedConstant

In the `$*CU` tree, `RakuAST::Lookup`, `X::AdHoc` and `Metamodel::ClassHOW` resolved to
`Declaration::External::Constant`. Rakudo resolves a name written with `::` through its
package, as a `Declaration::ResolvedConstant` holding the type object, and keeps
`External::Constant` for single-part names. mutsu now does the same, and the node's
`compile-time-value` answers the type object. Fixes #18116.
