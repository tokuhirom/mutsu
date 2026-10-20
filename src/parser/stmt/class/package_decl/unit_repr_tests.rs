use super::*;

// `unit class`/`unit role` extend to the end of the file, so their
// angle-bracket trait parsing (`is repr<...>`, `is ctype<...>`) is
// exercised here directly rather than via a t/*.t script — see
// t/is-repr-angle-bracket-trait.t for the block-form (`class`/`role`)
// coverage of the same underlying `parse_trait_angle_arg` mechanism.

#[test]
fn unit_class_accepts_repr_angle_trait() {
    let (_, stmt) = unit_module_stmt("unit class Foo is repr<CStruct>;").unwrap();
    let Stmt::ClassDecl { repr, .. } = stmt else {
        panic!("expected ClassDecl, got {stmt:?}");
    };
    assert_eq!(repr.as_deref(), Some("CStruct"));
}

#[test]
fn unit_class_accepts_ctype_angle_trait_as_custom_trait() {
    let (_, stmt) = unit_module_stmt("unit class Foo is ctype<long>;").unwrap();
    let Stmt::ClassDecl { custom_traits, .. } = stmt else {
        panic!("expected ClassDecl, got {stmt:?}");
    };
    assert!(
        custom_traits.iter().any(|(name, _)| name == "ctype"),
        "expected a 'ctype' custom trait, got {custom_traits:?}"
    );
}

#[test]
fn unit_role_accepts_ctype_angle_trait_as_custom_trait() {
    let (_, stmt) = unit_module_stmt("unit role Foo is ctype<long>;").unwrap();
    let Stmt::RoleDecl { custom_traits, .. } = stmt else {
        panic!("expected RoleDecl, got {stmt:?}");
    };
    assert!(
        custom_traits.iter().any(|(name, _)| name == "ctype"),
        "expected a 'ctype' custom trait, got {custom_traits:?}"
    );
}
