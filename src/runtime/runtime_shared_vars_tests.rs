use super::*;
use crate::meta_ns::MetaNs;
use crate::value::Value;

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
    interp.threads.shared_vars_active = true;
    interp
        .threads
        .shared_vars
        .set("url", Value::str("lane".to_string()));

    interp.threads.suppress_shared_publish = true;
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
        interp
            .threads
            .shared_vars
            .get("url")
            .and_then(|v| match v.view() {
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
    interp.threads.shared_vars_active = true;
    interp
        .threads
        .shared_vars
        .set("url", Value::str("lane".to_string()));

    interp.set_shared_var_sym("url", None, Value::str("assigned".to_string()));

    assert_eq!(
        interp
            .threads
            .shared_vars
            .get("url")
            .and_then(|v| match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            }),
        Some("assigned".to_string())
    );
    assert!(interp.is_shared_var_dirty("url"));
}

/// Entering a critical section refreshes only what ANOTHER thread committed.
/// A value this thread wrote itself (inside an earlier critical section) is in
/// its env already; pulling it back replaced a same-named lexical of the frame
/// now entering the section (Red's nested `-> :$guard!` blocks: the outer
/// block's `$guard` became the inner block's, written under `Lock.protect`).
#[test]
fn entering_a_critical_section_keeps_a_same_named_lexical_over_own_writes() {
    let mut interp = Interpreter::new();
    interp.threads.shared_vars_active = true;
    interp
        .threads
        .shared_vars
        .set("guard", Value::str("seed".to_string()));

    interp.enter_critical_section();
    interp.set_shared_var_sym("guard", None, Value::str("inner".to_string()));
    interp.leave_critical_section();

    // The outer frame's own `$guard`, entering a critical section later.
    interp.env.insert("guard".to_string(), Value::str("outer".to_string()));
    interp.enter_critical_section();

    assert_eq!(
        interp.env.get("guard").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        }),
        Some("outer".to_string()),
        "this thread's own earlier write must not replace the entering frame's lexical"
    );
    interp.leave_critical_section();
}


fn seed_dirty_atomic_lane(interp: &mut Interpreter, name: &str, value_key: &str) {
    interp.mark_atomic_var_seen(name);
    let name_key = MetaNs::AtomicName.owned_key_for_str(name);
    interp
        .threads
        .shared_vars
        .set(&name_key, Value::str(value_key.to_string()));
    interp.threads.shared_vars.set(value_key, Value::int(1));
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

fn env_str(interp: &Interpreter, name: &str) -> Option<String> {
    interp.env.get(name).and_then(|v| match v.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    })
}

/// A sigilless binding (`\type`) is bound once and never assigned, so the
/// store's entry for its bare name is only a snapshot of whichever frame last
/// bound that name. Pulling it into the env ahead of an inline `.protect`
/// block hid the running frame's own binding (#14867: Red's `Model.alias`
/// read `type` as `Mu` on its second call).
#[test]
fn protect_sync_leaves_a_sigilless_binding_alone() {
    let mut interp = Interpreter::new();
    interp.threads.shared_vars_active = true;
    interp
        .threads
        .shared_vars
        .set("\\type", Value::str("stale".to_string()));
    interp
        .threads
        .shared_vars
        .set("count", Value::str("shared".to_string()));
    interp.env.insert("\\type".to_string(), Value::str("live".to_string()));
    interp.env.insert("count".to_string(), Value::str("live".to_string()));

    interp.sync_shared_vars_for_names(["\\type", "count"]);

    assert_eq!(env_str(&interp, "\\type"), Some("live".to_string()));
    assert_eq!(env_str(&interp, "count"), Some("shared".to_string()));
}
