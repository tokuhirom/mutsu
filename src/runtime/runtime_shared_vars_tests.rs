use super::*;
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
