use super::*;
use crate::value::Value;

/// Minimal `ParamDef` for a plain scalar parameter (not slurpy, no default,
/// no traits) -- everything `mask_thread_redeclared_params` and the ordinary
/// call-binding path actually look at for this test. Mirrors
/// `parser::stmt::sub_param::helpers::make_param`.
fn scalar_param(name: &str) -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

/// Pins the fix for
/// `todo/tickets/thread-param-mask-leaks-on-panic-unwind.md`:
/// `ThreadParamMaskGuard` (`vm::vm_call_state_guard`) must undo exactly the
/// masking `mask_thread_redeclared_params` applied, even when a Rust panic
/// unwinds straight through the guarded call instead of returning normally --
/// the scenario a plain `unmask_thread_redeclared_params(...)` statement
/// (skipped entirely by an unwind) could not handle.
#[test]
fn thread_param_mask_guard_restores_on_panic_unwind() {
    let mut interp = Interpreter::new();
    interp.shared_vars_active = true;

    let param_defs = vec![scalar_param("$desc")];

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _guard = crate::vm::vm_call_state_guard::ThreadParamMaskGuard::new(
            &mut interp,
            param_defs.iter(),
        );
        assert!(
            interp.thread_redeclared_vars.borrow().contains("desc"),
            "the guard's constructor must mask the parameter's bare name"
        );
        assert!(
            interp.thread_param_shadow_vars.borrow().contains("desc"),
            "and the parameter-shadow companion set too"
        );
        // Simulate a genuine Rust panic raised deep inside the guarded call's
        // body, e.g. the integer-overflow/index-OOB panics the
        // t/*panic-boundary*.t suite triggers with `@a[2**64 - 1] = 1`.
        panic!("simulated panic mid-call, like an integer-overflow op");
    }));
    assert!(
        result.is_err(),
        "the panic must propagate out of catch_unwind (this test isn't about \
         swallowing it, only about what state survives)"
    );

    assert!(
        !interp.thread_redeclared_vars.borrow().contains("desc"),
        "ThreadParamMaskGuard::drop must undo the mask even when the guarded \
         call unwound via panic instead of returning normally"
    );
    assert!(
        !interp.thread_param_shadow_vars.borrow().contains("desc"),
        "same for the parameter-shadow companion set"
    );
}

/// The non-panicking case still works exactly as before: a normal drop (at
/// scope end here) undoes the mask too.
#[test]
fn thread_param_mask_guard_restores_on_normal_drop() {
    let mut interp = Interpreter::new();
    interp.shared_vars_active = true;

    let param_defs = vec![scalar_param("$desc")];

    {
        let _guard = crate::vm::vm_call_state_guard::ThreadParamMaskGuard::new(
            &mut interp,
            param_defs.iter(),
        );
        assert!(interp.thread_redeclared_vars.borrow().contains("desc"));
        assert!(interp.thread_param_shadow_vars.borrow().contains("desc"));
    }

    assert!(!interp.thread_redeclared_vars.borrow().contains("desc"));
    assert!(!interp.thread_param_shadow_vars.borrow().contains("desc"));
}

/// An ancestor frame's own mask on the same bare name must survive an inner
/// guard's drop -- `ThreadParamMask` records only the names THIS mask added
/// (see its doc comment), so unmasking must not remove a name that was
/// already masked by someone else.
#[test]
fn thread_param_mask_guard_does_not_disturb_an_ancestor_mask() {
    let mut interp = Interpreter::new();
    interp.shared_vars_active = true;
    interp
        .thread_redeclared_vars
        .borrow_mut()
        .insert("desc".to_string());
    interp
        .thread_param_shadow_vars
        .borrow_mut()
        .insert("desc".to_string());

    let param_defs = vec![scalar_param("$desc")];
    {
        let _guard = crate::vm::vm_call_state_guard::ThreadParamMaskGuard::new(
            &mut interp,
            param_defs.iter(),
        );
    }

    assert!(
        interp.thread_redeclared_vars.borrow().contains("desc"),
        "an ancestor frame's own mask on the same name must survive"
    );
    assert!(
        interp.thread_param_shadow_vars.borrow().contains("desc"),
        "same for the parameter-shadow companion set"
    );
}
/// The incidental locals -> env mirrors (the I/O pre-sync before
/// Say/Put/Print/Note, and the regex interpolation pre-sync) must reach
/// `env` WITHOUT touching the cross-thread lane. The lane is keyed by bare
/// name, so republishing whichever frame happens to be printing both
/// overwrites another live binding's entry and — via `mark_shared_var_dirty`
/// — makes the next `sync_shared_vars_to_env` pull that entry back over the
/// printing frame's own slot.
#[test]
fn suppressed_publish_writes_env_but_not_the_shared_lane() {
    let mut interp = Interpreter::new();
    interp.shared_vars_active = true;
    interp
        .shared_vars
        .declare("url", Value::str("lane".to_string()));

    interp.suppress_shared_publish = true;
    interp.set_shared_var_sym("url", None, Value::str("frame-local".to_string()));

    assert_eq!(
        interp.env.get("url").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        }),
        Some("frame-local".to_string()),
        "the mirror must still make the value visible by name in env"
    );
    assert_eq!(
        interp.shared_vars.get("url").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        }),
        Some("lane".to_string()),
        "the cross-thread lane must be left alone"
    );
    assert!(
        !interp.is_shared_var_dirty("url"),
        "a suppressed mirror must not mark the name dirty, or the next \
         sync pulls the lane back over the live binding"
    );
}

/// The same call with the flag clear is a genuine assignment and publishes.
#[test]
fn unsuppressed_write_publishes_to_the_shared_lane() {
    let mut interp = Interpreter::new();
    interp.shared_vars_active = true;
    interp
        .shared_vars
        .declare("url", Value::str("lane".to_string()));

    interp.set_shared_var_sym("url", None, Value::str("assigned".to_string()));

    assert_eq!(
        interp.shared_vars.get("url").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        }),
        Some("assigned".to_string())
    );
    assert!(interp.is_shared_var_dirty("url"));
}

fn seed_dirty_atomic_lane(interp: &mut Interpreter, name: &str, value_key: &str) {
    interp.mark_atomic_var_seen();
    let name_key = format!("__mutsu_atomic_name::{name}");
    interp
        .shared_vars
        .declare(&name_key, Value::str(value_key.to_string()));
    interp.shared_vars.declare(value_key, Value::int(1));
    interp.mark_shared_var_dirty(name);
    interp.mark_shared_var_dirty(value_key);
}

#[test]
fn reset_atomic_var_key_removes_only_retired_value_key_from_dirty_set() {
    let mut interp = Interpreter::new();
    let value_key = "__mutsu_atomic_value::unit-reset";
    seed_dirty_atomic_lane(&mut interp, "x", value_key);

    interp.reset_atomic_var_key("x");

    assert!(!interp.is_shared_var_dirty(value_key));
    assert!(
        interp.is_shared_var_dirty("x"),
        "the bare name remains load-bearing for published atomic seeds"
    );
}

#[test]
fn reset_atomic_var_key_decl_removes_only_retired_value_key_from_dirty_set() {
    let mut interp = Interpreter::new();
    let value_key = "__mutsu_atomic_value::unit-decl-reset";
    seed_dirty_atomic_lane(&mut interp, "x", value_key);

    interp.reset_atomic_var_key_decl("x");

    assert!(!interp.is_shared_var_dirty(value_key));
    assert!(
        interp.is_shared_var_dirty("x"),
        "declaration retirement must not clear the bare name"
    );
}
