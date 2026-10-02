//! `Interpreter::registry_has_destroy_methods`, which decides whether the
//! program-end cycle collect runs at all (#10961 rewrote it as a method-table
//! scan).

use super::*;

fn has_destroy_after(code: &str) -> bool {
    let mut interp = Interpreter::new();
    interp.run(code).expect("program runs");
    interp.registry_has_destroy_methods()
}

#[test]
fn no_user_destroy_means_none() {
    assert!(!has_destroy_after(
        "class NoDestroyA { method m { 1 } }; NoDestroyA.new.m;"
    ));
}

#[test]
fn a_class_destroy_is_found() {
    assert!(has_destroy_after(
        "class WithDestroyA { submethod DESTROY { } }; WithDestroyA.new;"
    ));
    assert!(has_destroy_after(
        "class WithDestroyB { method DESTROY { } }; WithDestroyB.new;"
    ));
}

#[test]
fn a_role_destroy_is_found() {
    assert!(has_destroy_after(
        "role RoleDestroyA { submethod DESTROY { } }; class UsesRoleDestroyA does RoleDestroyA { }; UsesRoleDestroyA.new;"
    ));
}
