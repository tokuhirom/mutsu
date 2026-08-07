# Role-composed and augment-declared methods keep their `is DEPRECATED` message

Found while scoping ADR-0019 D3 (which method declarations get walked
independently in three places: the class body, role body, and `augment
class` — each building a `MethodDef` from `Stmt::MethodDecl` by hand). The
scoping pass flagged that `registration_role_method.rs`'s
`role_body_method_decl` and `registration_class_augment.rs`'s
`augment_class` both destructured `Stmt::MethodDecl::deprecated_message` and
discarded it, hard-coding `MethodDef.deprecated_message: None` instead —
unlike the class-body walker (`registration_class_body_method.rs`), which
threads it through correctly.

Confirmed against `raku` as a real behavior gap, not just an internal
inconsistency:

```raku
role R { method foo() is DEPRECATED('use bar instead') { } }
class C does R { }
C.new.foo;
```

`raku` prints the usual `Saw 1 occurrence of deprecated code.` report;
mutsu silently ran the method with no report at all. Same gap for
`augment class C { method foo() is DEPRECATED(...) { } }`.

Both sites now thread `deprecated_message` through to the constructed
`MethodDef`, matching the class-body walker. Verified with the full `t/`
suite, the whitelisted `roast/S02-types/isDEPRECATED.t`, and every
`S12-methods`/`S14-roles` whitelisted file (52 files, 938 tests), all green.
A new `t/role-augment-method-deprecated-message.t` pins both cases via
`Deprecation.report`.

Note: the reported attribution (`Method foo (from C)` vs. rakudo's `(from
R)` for the role case) is a separate, pre-existing, purely cosmetic
discrepancy in how the deprecation report names the declaring
package — unrelated to this fix, which is about the message surviving at
all.
