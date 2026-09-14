# Roles accept qualified `is` parents

Block and unit role declarations now parse the complete qualified name after
`is`, such as `Metamodel::SubsetHOW`. This lets Red's
`MetamodelX::Red::SubModelHOW` load past its former `Unknown role: Metamodel`
failure.

Pinned by `t/oo/role/role-qualified-is-parent.t` and its
`t/lib/QualifiedRoleIsParent.rakumod` fixture. The remaining Red runtime and
relationship failures are independent leads in
[#7988](https://github.com/tokuhirom/mutsu/issues/7988).
